{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | See <https://github.com/brendanhay/amazonka/issues/418>
--
-- Heavy Sigh.
--
module Network.AWS.CloudFormation.Ext
  ( DescribeStackEvents'(..)
  , DescribeStackEventsResponse'(..)
  , dserspNextToken
  , dserspStackEvents
  , dserspResponseStatus
  , CustomStackEvent(..)
  , cseLogicalResourceId
  , csePhysicalResourceId
  , cseResourceType
  , cseResourceStatusReason
  , cseResourceProperties
  , cseResourceStatus
  , cseClientRequestToken
  , cseStackId
  , cseEventId
  , cseStackName
  , cseTimestamp
  ) where

import Network.AWS.CloudFormation
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

newtype DescribeStackEvents' = DescribeStackEvents'
  { unDescribeStackEvents' :: DescribeStackEvents
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic, Hashable, NFData, ToHeaders, ToPath)

instance AWSPager DescribeStackEvents' where
  page (DescribeStackEvents' rq) rs
    | stop (rs ^. dserspNextToken) = Nothing
    | stop (rs ^. dserspStackEvents) = Nothing
    | otherwise =
      Just $ DescribeStackEvents' $ rq & dseNextToken .~ rs ^. dserspNextToken

instance AWSRequest DescribeStackEvents' where
  type Rs DescribeStackEvents' = DescribeStackEventsResponse'
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper "DescribeStackEventsResult" $ \s _ x ->
      DescribeStackEventsResponse'
        <$> (x .@? "NextToken")
        <*> (x .@? "StackEvents" .!@ mempty >>= may (parseXMLList "member"))
        <*> pure (fromEnum s)

instance ToQuery DescribeStackEvents' where
  toQuery (DescribeStackEvents' x) = toQuery x

data DescribeStackEventsResponse'
  = DescribeStackEventsResponse'
  { _dserspNextToken :: !(Maybe Text)
  , _dserspStackEvents :: !(Maybe [CustomStackEvent])
  , _dserspResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- deriving instance NFData DescribeStackEventsResponse'

dserspNextToken :: Lens' DescribeStackEventsResponse' (Maybe Text)
dserspNextToken = lens _dserspNextToken (\ s a -> s{_dserspNextToken = a})

dserspStackEvents :: Lens' DescribeStackEventsResponse' [CustomStackEvent]
dserspStackEvents = lens _dserspStackEvents (\ s a -> s{_dserspStackEvents = a}) . _Default . _Coerce

dserspResponseStatus :: Lens' DescribeStackEventsResponse' Int
dserspResponseStatus = lens _dserspResponseStatus (\ s a -> s{_dserspResponseStatus = a})

data CustomStackEvent
  = CustomStackEvent
  { _cseLogicalResourceId :: !(Maybe Text)
  , _csePhysicalResourceId :: !(Maybe Text)
  , _cseResourceType :: !(Maybe Text)
  , _cseResourceStatusReason :: !(Maybe Text)
  , _cseResourceProperties :: !(Maybe Text)
  , _cseResourceStatus :: !(Maybe Text)
    -- ^ This is the reason this file exists
  , _cseClientRequestToken :: !(Maybe Text)
  , _cseStackId :: !Text
  , _cseEventId :: !Text
  , _cseStackName :: !Text
  , _cseTimestamp :: !ISO8601
  } deriving (Eq,Read,Show,Data,Typeable,Generic)

cseLogicalResourceId :: Lens' CustomStackEvent (Maybe Text)
cseLogicalResourceId = lens _cseLogicalResourceId (\ s a -> s{_cseLogicalResourceId = a})

csePhysicalResourceId :: Lens' CustomStackEvent (Maybe Text)
csePhysicalResourceId = lens _csePhysicalResourceId (\ s a -> s{_csePhysicalResourceId = a})

cseResourceType :: Lens' CustomStackEvent (Maybe Text)
cseResourceType = lens _cseResourceType (\ s a -> s{_cseResourceType = a})

cseResourceStatusReason :: Lens' CustomStackEvent (Maybe Text)
cseResourceStatusReason = lens _cseResourceStatusReason (\ s a -> s{_cseResourceStatusReason = a})

cseResourceProperties :: Lens' CustomStackEvent (Maybe Text)
cseResourceProperties = lens _cseResourceProperties (\ s a -> s{_cseResourceProperties = a})

cseResourceStatus :: Lens' CustomStackEvent (Maybe Text)
cseResourceStatus = lens _cseResourceStatus (\ s a -> s{_cseResourceStatus = a})

cseClientRequestToken :: Lens' CustomStackEvent (Maybe Text)
cseClientRequestToken = lens _cseClientRequestToken (\ s a -> s{_cseClientRequestToken = a})

cseStackId :: Lens' CustomStackEvent Text
cseStackId = lens _cseStackId (\ s a -> s{_cseStackId = a})

cseEventId :: Lens' CustomStackEvent Text
cseEventId = lens _cseEventId (\ s a -> s{_cseEventId = a})

cseStackName :: Lens' CustomStackEvent Text
cseStackName = lens _cseStackName (\ s a -> s{_cseStackName = a})

cseTimestamp :: Lens' CustomStackEvent UTCTime
cseTimestamp = lens _cseTimestamp (\ s a -> s{_cseTimestamp = a}) . _Time

instance FromXML CustomStackEvent where
  parseXML x = CustomStackEvent
    <$> (x .@? "LogicalResourceId")
    <*> (x .@? "PhysicalResourceId")
    <*> (x .@? "ResourceType")
    <*> (x .@? "ResourceStatusReason")
    <*> (x .@? "ResourceProperties")
    <*> (x .@? "ResourceStatus")
    <*> (x .@? "ClientRequestToken")
    <*> (x .@ "StackId")
    <*> (x .@ "EventId")
    <*> (x .@ "StackName")
    <*> (x .@ "Timestamp")

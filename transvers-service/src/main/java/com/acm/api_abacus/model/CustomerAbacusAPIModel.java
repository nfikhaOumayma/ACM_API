/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * {@link CustomerAbacusAPIModel} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5529677769256824051L;

	/** The customer ID. */
	private int customerID;

	/** The customer type. */
	private int customerType;

	/** The name. */
	private String name;

	/** The number. */
	private String number;

	/** The branch ID. */
	private int branchID;

	/** The customer address. */
	private List<CustomerAbacusAPIModelCustomerAddress> customerAddress;

	/** The persons. */
	private List<CustomerAbacusAPIModelPerson> persons;

	/** The surveys. */
	private List<CustomerAbacusAPIModelSurvey> surveys;

	/** The alt name. */
	private String altName;

	/** The initial product id. */
	private int initialProductId;

	/** The default CU account portfolio ID. */
	private int defaultCUAccountPortfolioID;

	/** The status. */
	private int status;

	/** The correspondence name. */
	private String correspondenceName;

	/** The number of active loans. */
	private int numberOfActiveLoans;

	/** The customer address string. */
	private String customerAddressString;

	/** The community customers. */
	private String communityCustomers;

	/** The analysis links. */
	private List<Object> analysisLinks;

	/** The track accs individually. */
	private boolean trackAccsIndividually;

	/** The customer has community loans. */
	private boolean customerHasCommunityLoans;

	/** The sms consent given. */
	private String smsConsentGiven;

	/** The sms loan approval alerts. */
	private String smsLoanApprovalAlerts;

	/** The sms transaction alerts. */
	private String smsTransactionAlerts;

	/** The date joined. */
	private Date dateJoined;

	/** The misc personal ID. */
	private int miscPersonalID;

	/** The misc addresss ID. */
	private int miscAddresssID;

	/** The credit rating ID. */
	private int creditRatingID;

	/** The village bank ID. */
	private int villageBankID;

	/** The icb consent given. */
	private boolean icbConsentGiven;

	/** The initial batch ID. */
	private int initialBatchID;

	/** The initial fee ID. */
	private int initialFeeID;

	/** The scv account code ID. */
	private int scvAccountCodeID;

	/** The statement frequency ID. */
	private int statementFrequencyID;

	/** The statement frequency num. */
	private int statementFrequencyNum;

	/** The experian consent given. */
	private boolean experianConsentGiven;

	/** The equifax consent given. */
	private boolean equifaxConsentGiven;

	/** The sms transaction types. */
	private int smsTransactionTypes;

	/** The sms transaction sources. */
	private int smsTransactionSources;

	/** The override loan amount limits. */
	private boolean overrideLoanAmountLimits;

	/** The bankaccounts. */
	private List<Object> bankaccounts;

	/** The customer loan limits. */
	private List<CustomerAbacusAPIModelCustomerLoanLimit> customerLoanLimits;

	/** The customer relationships. */
	private List<Object> customerRelationships;

	/** The customer black listing. */
	private List<Object> customerBlackListing;

	/** The regular statements. */
	private boolean regularStatements;

	/** The number of communities. */
	private int numberOfCommunities;

	/** The organisation. */
	private CustomerAbacusAPIModelOrganisation organisation;

	/** The closed. */
	private boolean closed;

	/** The closed reason ID. */
	private int closedReasonID;

	/** The communities. */
	private List<Object> communities;

	/** The reschedule loans. */
	private String rescheduleLoans;

	/** The original village bank ID. */
	private String originalVillageBankID;

	/** The internet banking registration. */
	private CustomerAbacusAPIModelInternetBankingRegistration internetBankingRegistration;

	/** The ib comm consent. */
	private boolean ibCommConsent;

	/** The web hook queue not allowed. */
	private String webHookQueueNotAllowed;

	/** The address code. */
	private int addressCode;

	/** The customer number. */
	private String customerNumber;

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public int getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(int customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the customer type.
	 *
	 * @return the customerType
	 */
	public int getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 *
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(int customerType) {

		this.customerType = customerType;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the number.
	 *
	 * @return the number
	 */
	public String getNumber() {

		return number;
	}

	/**
	 * Sets the number.
	 *
	 * @param number the number to set
	 */
	public void setNumber(String number) {

		this.number = number;
	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branchID
	 */
	public int getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the branchID to set
	 */
	public void setBranchID(int branchID) {

		this.branchID = branchID;
	}

	/**
	 * Gets the customer address.
	 *
	 * @return the customerAddress
	 */
	public List<CustomerAbacusAPIModelCustomerAddress> getCustomerAddress() {

		return customerAddress;
	}

	/**
	 * Sets the customer address.
	 *
	 * @param customerAddress the customerAddress to set
	 */
	public void setCustomerAddress(List<CustomerAbacusAPIModelCustomerAddress> customerAddress) {

		this.customerAddress = customerAddress;
	}

	/**
	 * Gets the persons.
	 *
	 * @return the persons
	 */
	public List<CustomerAbacusAPIModelPerson> getPersons() {

		return persons;
	}

	/**
	 * Sets the persons.
	 *
	 * @param persons the persons to set
	 */
	public void setPersons(List<CustomerAbacusAPIModelPerson> persons) {

		this.persons = persons;
	}

	/**
	 * Gets the surveys.
	 *
	 * @return the surveys
	 */
	public List<CustomerAbacusAPIModelSurvey> getSurveys() {

		return surveys;
	}

	/**
	 * Sets the surveys.
	 *
	 * @param surveys the surveys to set
	 */
	public void setSurveys(List<CustomerAbacusAPIModelSurvey> surveys) {

		this.surveys = surveys;
	}

	/**
	 * Gets the alt name.
	 *
	 * @return the altName
	 */
	public String getAltName() {

		return altName;
	}

	/**
	 * Sets the alt name.
	 *
	 * @param altName the altName to set
	 */
	public void setAltName(String altName) {

		this.altName = altName;
	}

	/**
	 * Gets the initial product id.
	 *
	 * @return the initialProductId
	 */
	public int getInitialProductId() {

		return initialProductId;
	}

	/**
	 * Sets the initial product id.
	 *
	 * @param initialProductId the initialProductId to set
	 */
	public void setInitialProductId(int initialProductId) {

		this.initialProductId = initialProductId;
	}

	/**
	 * Gets the default CU account portfolio ID.
	 *
	 * @return the defaultCUAccountPortfolioID
	 */
	public int getDefaultCUAccountPortfolioID() {

		return defaultCUAccountPortfolioID;
	}

	/**
	 * Sets the default CU account portfolio ID.
	 *
	 * @param defaultCUAccountPortfolioID the defaultCUAccountPortfolioID to set
	 */
	public void setDefaultCUAccountPortfolioID(int defaultCUAccountPortfolioID) {

		this.defaultCUAccountPortfolioID = defaultCUAccountPortfolioID;
	}

	/**
	 * Gets the status.
	 *
	 * @return the status
	 */
	public int getStatus() {

		return status;
	}

	/**
	 * Sets the status.
	 *
	 * @param status the status to set
	 */
	public void setStatus(int status) {

		this.status = status;
	}

	/**
	 * Gets the correspondence name.
	 *
	 * @return the correspondenceName
	 */
	public String getCorrespondenceName() {

		return correspondenceName;
	}

	/**
	 * Sets the correspondence name.
	 *
	 * @param correspondenceName the correspondenceName to set
	 */
	public void setCorrespondenceName(String correspondenceName) {

		this.correspondenceName = correspondenceName;
	}

	/**
	 * Gets the number of active loans.
	 *
	 * @return the numberOfActiveLoans
	 */
	public int getNumberOfActiveLoans() {

		return numberOfActiveLoans;
	}

	/**
	 * Sets the number of active loans.
	 *
	 * @param numberOfActiveLoans the numberOfActiveLoans to set
	 */
	public void setNumberOfActiveLoans(int numberOfActiveLoans) {

		this.numberOfActiveLoans = numberOfActiveLoans;
	}

	/**
	 * Gets the customer address string.
	 *
	 * @return the customerAddressString
	 */
	public String getCustomerAddressString() {

		return customerAddressString;
	}

	/**
	 * Sets the customer address string.
	 *
	 * @param customerAddressString the customerAddressString to set
	 */
	public void setCustomerAddressString(String customerAddressString) {

		this.customerAddressString = customerAddressString;
	}

	/**
	 * Gets the community customers.
	 *
	 * @return the communityCustomers
	 */
	public String getCommunityCustomers() {

		return communityCustomers;
	}

	/**
	 * Sets the community customers.
	 *
	 * @param communityCustomers the communityCustomers to set
	 */
	public void setCommunityCustomers(String communityCustomers) {

		this.communityCustomers = communityCustomers;
	}

	/**
	 * Gets the analysis links.
	 *
	 * @return the analysisLinks
	 */
	public List<Object> getAnalysisLinks() {

		return analysisLinks;
	}

	/**
	 * Sets the analysis links.
	 *
	 * @param analysisLinks the analysisLinks to set
	 */
	public void setAnalysisLinks(List<Object> analysisLinks) {

		this.analysisLinks = analysisLinks;
	}

	/**
	 * Checks if is track accs individually.
	 *
	 * @return the trackAccsIndividually
	 */
	public boolean isTrackAccsIndividually() {

		return trackAccsIndividually;
	}

	/**
	 * Sets the track accs individually.
	 *
	 * @param trackAccsIndividually the trackAccsIndividually to set
	 */
	public void setTrackAccsIndividually(boolean trackAccsIndividually) {

		this.trackAccsIndividually = trackAccsIndividually;
	}

	/**
	 * Checks if is customer has community loans.
	 *
	 * @return the customerHasCommunityLoans
	 */
	public boolean isCustomerHasCommunityLoans() {

		return customerHasCommunityLoans;
	}

	/**
	 * Sets the customer has community loans.
	 *
	 * @param customerHasCommunityLoans the customerHasCommunityLoans to set
	 */
	public void setCustomerHasCommunityLoans(boolean customerHasCommunityLoans) {

		this.customerHasCommunityLoans = customerHasCommunityLoans;
	}

	/**
	 * Gets the sms consent given.
	 *
	 * @return the smsConsentGiven
	 */
	public String getSmsConsentGiven() {

		return smsConsentGiven;
	}

	/**
	 * Sets the sms consent given.
	 *
	 * @param smsConsentGiven the smsConsentGiven to set
	 */
	public void setSmsConsentGiven(String smsConsentGiven) {

		this.smsConsentGiven = smsConsentGiven;
	}

	/**
	 * Gets the sms loan approval alerts.
	 *
	 * @return the smsLoanApprovalAlerts
	 */
	public String getSmsLoanApprovalAlerts() {

		return smsLoanApprovalAlerts;
	}

	/**
	 * Sets the sms loan approval alerts.
	 *
	 * @param smsLoanApprovalAlerts the smsLoanApprovalAlerts to set
	 */
	public void setSmsLoanApprovalAlerts(String smsLoanApprovalAlerts) {

		this.smsLoanApprovalAlerts = smsLoanApprovalAlerts;
	}

	/**
	 * Gets the sms transaction alerts.
	 *
	 * @return the smsTransactionAlerts
	 */
	public String getSmsTransactionAlerts() {

		return smsTransactionAlerts;
	}

	/**
	 * Sets the sms transaction alerts.
	 *
	 * @param smsTransactionAlerts the smsTransactionAlerts to set
	 */
	public void setSmsTransactionAlerts(String smsTransactionAlerts) {

		this.smsTransactionAlerts = smsTransactionAlerts;
	}

	/**
	 * Gets the date joined.
	 *
	 * @return the dateJoined
	 */
	public Date getDateJoined() {

		return dateJoined;
	}

	/**
	 * Sets the date joined.
	 *
	 * @param dateJoined the dateJoined to set
	 */
	public void setDateJoined(Date dateJoined) {

		this.dateJoined = dateJoined;
	}

	/**
	 * Gets the misc personal ID.
	 *
	 * @return the miscPersonalID
	 */
	public int getMiscPersonalID() {

		return miscPersonalID;
	}

	/**
	 * Sets the misc personal ID.
	 *
	 * @param miscPersonalID the miscPersonalID to set
	 */
	public void setMiscPersonalID(int miscPersonalID) {

		this.miscPersonalID = miscPersonalID;
	}

	/**
	 * Gets the misc addresss ID.
	 *
	 * @return the miscAddresssID
	 */
	public int getMiscAddresssID() {

		return miscAddresssID;
	}

	/**
	 * Sets the misc addresss ID.
	 *
	 * @param miscAddresssID the miscAddresssID to set
	 */
	public void setMiscAddresssID(int miscAddresssID) {

		this.miscAddresssID = miscAddresssID;
	}

	/**
	 * Gets the credit rating ID.
	 *
	 * @return the creditRatingID
	 */
	public int getCreditRatingID() {

		return creditRatingID;
	}

	/**
	 * Sets the credit rating ID.
	 *
	 * @param creditRatingID the creditRatingID to set
	 */
	public void setCreditRatingID(int creditRatingID) {

		this.creditRatingID = creditRatingID;
	}

	/**
	 * Gets the village bank ID.
	 *
	 * @return the villageBankID
	 */
	public int getVillageBankID() {

		return villageBankID;
	}

	/**
	 * Sets the village bank ID.
	 *
	 * @param villageBankID the villageBankID to set
	 */
	public void setVillageBankID(int villageBankID) {

		this.villageBankID = villageBankID;
	}

	/**
	 * Checks if is icb consent given.
	 *
	 * @return the icbConsentGiven
	 */
	public boolean isIcbConsentGiven() {

		return icbConsentGiven;
	}

	/**
	 * Sets the icb consent given.
	 *
	 * @param icbConsentGiven the icbConsentGiven to set
	 */
	public void setIcbConsentGiven(boolean icbConsentGiven) {

		this.icbConsentGiven = icbConsentGiven;
	}

	/**
	 * Gets the initial batch ID.
	 *
	 * @return the initialBatchID
	 */
	public int getInitialBatchID() {

		return initialBatchID;
	}

	/**
	 * Sets the initial batch ID.
	 *
	 * @param initialBatchID the initialBatchID to set
	 */
	public void setInitialBatchID(int initialBatchID) {

		this.initialBatchID = initialBatchID;
	}

	/**
	 * Gets the initial fee ID.
	 *
	 * @return the initialFeeID
	 */
	public int getInitialFeeID() {

		return initialFeeID;
	}

	/**
	 * Sets the initial fee ID.
	 *
	 * @param initialFeeID the initialFeeID to set
	 */
	public void setInitialFeeID(int initialFeeID) {

		this.initialFeeID = initialFeeID;
	}

	/**
	 * Gets the scv account code ID.
	 *
	 * @return the scvAccountCodeID
	 */
	public int getScvAccountCodeID() {

		return scvAccountCodeID;
	}

	/**
	 * Sets the scv account code ID.
	 *
	 * @param scvAccountCodeID the scvAccountCodeID to set
	 */
	public void setScvAccountCodeID(int scvAccountCodeID) {

		this.scvAccountCodeID = scvAccountCodeID;
	}

	/**
	 * Gets the statement frequency ID.
	 *
	 * @return the statementFrequencyID
	 */
	public int getStatementFrequencyID() {

		return statementFrequencyID;
	}

	/**
	 * Sets the statement frequency ID.
	 *
	 * @param statementFrequencyID the statementFrequencyID to set
	 */
	public void setStatementFrequencyID(int statementFrequencyID) {

		this.statementFrequencyID = statementFrequencyID;
	}

	/**
	 * Gets the statement frequency num.
	 *
	 * @return the statementFrequencyNum
	 */
	public int getStatementFrequencyNum() {

		return statementFrequencyNum;
	}

	/**
	 * Sets the statement frequency num.
	 *
	 * @param statementFrequencyNum the statementFrequencyNum to set
	 */
	public void setStatementFrequencyNum(int statementFrequencyNum) {

		this.statementFrequencyNum = statementFrequencyNum;
	}

	/**
	 * Checks if is experian consent given.
	 *
	 * @return the experianConsentGiven
	 */
	public boolean isExperianConsentGiven() {

		return experianConsentGiven;
	}

	/**
	 * Sets the experian consent given.
	 *
	 * @param experianConsentGiven the experianConsentGiven to set
	 */
	public void setExperianConsentGiven(boolean experianConsentGiven) {

		this.experianConsentGiven = experianConsentGiven;
	}

	/**
	 * Checks if is equifax consent given.
	 *
	 * @return the equifaxConsentGiven
	 */
	public boolean isEquifaxConsentGiven() {

		return equifaxConsentGiven;
	}

	/**
	 * Sets the equifax consent given.
	 *
	 * @param equifaxConsentGiven the equifaxConsentGiven to set
	 */
	public void setEquifaxConsentGiven(boolean equifaxConsentGiven) {

		this.equifaxConsentGiven = equifaxConsentGiven;
	}

	/**
	 * Gets the sms transaction types.
	 *
	 * @return the smsTransactionTypes
	 */
	public int getSmsTransactionTypes() {

		return smsTransactionTypes;
	}

	/**
	 * Sets the sms transaction types.
	 *
	 * @param smsTransactionTypes the smsTransactionTypes to set
	 */
	public void setSmsTransactionTypes(int smsTransactionTypes) {

		this.smsTransactionTypes = smsTransactionTypes;
	}

	/**
	 * Gets the sms transaction sources.
	 *
	 * @return the smsTransactionSources
	 */
	public int getSmsTransactionSources() {

		return smsTransactionSources;
	}

	/**
	 * Sets the sms transaction sources.
	 *
	 * @param smsTransactionSources the smsTransactionSources to set
	 */
	public void setSmsTransactionSources(int smsTransactionSources) {

		this.smsTransactionSources = smsTransactionSources;
	}

	/**
	 * Checks if is override loan amount limits.
	 *
	 * @return the overrideLoanAmountLimits
	 */
	public boolean isOverrideLoanAmountLimits() {

		return overrideLoanAmountLimits;
	}

	/**
	 * Sets the override loan amount limits.
	 *
	 * @param overrideLoanAmountLimits the overrideLoanAmountLimits to set
	 */
	public void setOverrideLoanAmountLimits(boolean overrideLoanAmountLimits) {

		this.overrideLoanAmountLimits = overrideLoanAmountLimits;
	}

	/**
	 * Gets the bankaccounts.
	 *
	 * @return the bankaccounts
	 */
	public List<Object> getBankaccounts() {

		return bankaccounts;
	}

	/**
	 * Sets the bankaccounts.
	 *
	 * @param bankaccounts the bankaccounts to set
	 */
	public void setBankaccounts(List<Object> bankaccounts) {

		this.bankaccounts = bankaccounts;
	}

	/**
	 * Gets the customer loan limits.
	 *
	 * @return the customerLoanLimits
	 */
	public List<CustomerAbacusAPIModelCustomerLoanLimit> getCustomerLoanLimits() {

		return customerLoanLimits;
	}

	/**
	 * Sets the customer loan limits.
	 *
	 * @param customerLoanLimits the customerLoanLimits to set
	 */
	public void setCustomerLoanLimits(
			List<CustomerAbacusAPIModelCustomerLoanLimit> customerLoanLimits) {

		this.customerLoanLimits = customerLoanLimits;
	}

	/**
	 * Gets the customer relationships.
	 *
	 * @return the customerRelationships
	 */
	public List<Object> getCustomerRelationships() {

		return customerRelationships;
	}

	/**
	 * Sets the customer relationships.
	 *
	 * @param customerRelationships the customerRelationships to set
	 */
	public void setCustomerRelationships(List<Object> customerRelationships) {

		this.customerRelationships = customerRelationships;
	}

	/**
	 * Gets the customer black listing.
	 *
	 * @return the customerBlackListing
	 */
	public List<Object> getCustomerBlackListing() {

		return customerBlackListing;
	}

	/**
	 * Sets the customer black listing.
	 *
	 * @param customerBlackListing the customerBlackListing to set
	 */
	public void setCustomerBlackListing(List<Object> customerBlackListing) {

		this.customerBlackListing = customerBlackListing;
	}

	/**
	 * Checks if is regular statements.
	 *
	 * @return the regularStatements
	 */
	public boolean isRegularStatements() {

		return regularStatements;
	}

	/**
	 * Sets the regular statements.
	 *
	 * @param regularStatements the regularStatements to set
	 */
	public void setRegularStatements(boolean regularStatements) {

		this.regularStatements = regularStatements;
	}

	/**
	 * Gets the number of communities.
	 *
	 * @return the numberOfCommunities
	 */
	public int getNumberOfCommunities() {

		return numberOfCommunities;
	}

	/**
	 * Sets the number of communities.
	 *
	 * @param numberOfCommunities the numberOfCommunities to set
	 */
	public void setNumberOfCommunities(int numberOfCommunities) {

		this.numberOfCommunities = numberOfCommunities;
	}

	/**
	 * Gets the organisation.
	 *
	 * @return the organisation
	 */
	public CustomerAbacusAPIModelOrganisation getOrganisation() {

		return organisation;
	}

	/**
	 * Sets the organisation.
	 *
	 * @param organisation the organisation to set
	 */
	public void setOrganisation(CustomerAbacusAPIModelOrganisation organisation) {

		this.organisation = organisation;
	}

	/**
	 * Checks if is closed.
	 *
	 * @return the closed
	 */
	public boolean isClosed() {

		return closed;
	}

	/**
	 * Sets the closed.
	 *
	 * @param closed the closed to set
	 */
	public void setClosed(boolean closed) {

		this.closed = closed;
	}

	/**
	 * Gets the closed reason ID.
	 *
	 * @return the closedReasonID
	 */
	public int getClosedReasonID() {

		return closedReasonID;
	}

	/**
	 * Sets the closed reason ID.
	 *
	 * @param closedReasonID the closedReasonID to set
	 */
	public void setClosedReasonID(int closedReasonID) {

		this.closedReasonID = closedReasonID;
	}

	/**
	 * Gets the communities.
	 *
	 * @return the communities
	 */
	public List<Object> getCommunities() {

		return communities;
	}

	/**
	 * Sets the communities.
	 *
	 * @param communities the communities to set
	 */
	public void setCommunities(List<Object> communities) {

		this.communities = communities;
	}

	/**
	 * Gets the reschedule loans.
	 *
	 * @return the rescheduleLoans
	 */
	public String getRescheduleLoans() {

		return rescheduleLoans;
	}

	/**
	 * Sets the reschedule loans.
	 *
	 * @param rescheduleLoans the rescheduleLoans to set
	 */
	public void setRescheduleLoans(String rescheduleLoans) {

		this.rescheduleLoans = rescheduleLoans;
	}

	/**
	 * Gets the original village bank ID.
	 *
	 * @return the originalVillageBankID
	 */
	public String getOriginalVillageBankID() {

		return originalVillageBankID;
	}

	/**
	 * Sets the original village bank ID.
	 *
	 * @param originalVillageBankID the originalVillageBankID to set
	 */
	public void setOriginalVillageBankID(String originalVillageBankID) {

		this.originalVillageBankID = originalVillageBankID;
	}

	/**
	 * Gets the internet banking registration.
	 *
	 * @return the internetBankingRegistration
	 */
	public CustomerAbacusAPIModelInternetBankingRegistration getInternetBankingRegistration() {

		return internetBankingRegistration;
	}

	/**
	 * Sets the internet banking registration.
	 *
	 * @param internetBankingRegistration the internetBankingRegistration to set
	 */
	public void setInternetBankingRegistration(
			CustomerAbacusAPIModelInternetBankingRegistration internetBankingRegistration) {

		this.internetBankingRegistration = internetBankingRegistration;
	}

	/**
	 * Checks if is ib comm consent.
	 *
	 * @return the ibCommConsent
	 */
	public boolean isIbCommConsent() {

		return ibCommConsent;
	}

	/**
	 * Sets the ib comm consent.
	 *
	 * @param ibCommConsent the ibCommConsent to set
	 */
	public void setIbCommConsent(boolean ibCommConsent) {

		this.ibCommConsent = ibCommConsent;
	}

	/**
	 * Gets the web hook queue not allowed.
	 *
	 * @return the webHookQueueNotAllowed
	 */
	public String getWebHookQueueNotAllowed() {

		return webHookQueueNotAllowed;
	}

	/**
	 * Sets the web hook queue not allowed.
	 *
	 * @param webHookQueueNotAllowed the webHookQueueNotAllowed to set
	 */
	public void setWebHookQueueNotAllowed(String webHookQueueNotAllowed) {

		this.webHookQueueNotAllowed = webHookQueueNotAllowed;
	}

	/**
	 * Gets the address code.
	 *
	 * @return the addressCode
	 */
	public int getAddressCode() {

		return addressCode;
	}

	/**
	 * Sets the address code.
	 *
	 * @param addressCode the addressCode to set
	 */
	public void setAddressCode(int addressCode) {

		this.addressCode = addressCode;
	}

	/**
	 * Gets the customer number.
	 *
	 * @return the customerNumber
	 */
	public String getCustomerNumber() {

		return customerNumber;
	}

	/**
	 * Sets the customer number.
	 *
	 * @param customerNumber the customerNumber to set
	 */
	public void setCustomerNumber(String customerNumber) {

		this.customerNumber = customerNumber;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModel [customerID=" + customerID + ", customerType=" + customerType
				+ ", name=" + name + ", number=" + number + ", branchID=" + branchID
				+ ", customerAddress=" + customerAddress + ", persons=" + persons + ", surveys="
				+ surveys + ", altName=" + altName + ", initialProductId=" + initialProductId
				+ ", defaultCUAccountPortfolioID=" + defaultCUAccountPortfolioID + ", status="
				+ status + ", correspondenceName=" + correspondenceName + ", numberOfActiveLoans="
				+ numberOfActiveLoans + ", customerAddressString=" + customerAddressString
				+ ", communityCustomers=" + communityCustomers + ", analysisLinks=" + analysisLinks
				+ ", trackAccsIndividually=" + trackAccsIndividually
				+ ", customerHasCommunityLoans=" + customerHasCommunityLoans + ", smsConsentGiven="
				+ smsConsentGiven + ", smsLoanApprovalAlerts=" + smsLoanApprovalAlerts
				+ ", smsTransactionAlerts=" + smsTransactionAlerts + ", dateJoined=" + dateJoined
				+ ", miscPersonalID=" + miscPersonalID + ", miscAddresssID=" + miscAddresssID
				+ ", creditRatingID=" + creditRatingID + ", villageBankID=" + villageBankID
				+ ", icbConsentGiven=" + icbConsentGiven + ", initialBatchID=" + initialBatchID
				+ ", initialFeeID=" + initialFeeID + ", scvAccountCodeID=" + scvAccountCodeID
				+ ", statementFrequencyID=" + statementFrequencyID + ", statementFrequencyNum="
				+ statementFrequencyNum + ", experianConsentGiven=" + experianConsentGiven
				+ ", equifaxConsentGiven=" + equifaxConsentGiven + ", smsTransactionTypes="
				+ smsTransactionTypes + ", smsTransactionSources=" + smsTransactionSources
				+ ", overrideLoanAmountLimits=" + overrideLoanAmountLimits + ", bankaccounts="
				+ bankaccounts + ", customerLoanLimits=" + customerLoanLimits
				+ ", customerRelationships=" + customerRelationships + ", customerBlackListing="
				+ customerBlackListing + ", regularStatements=" + regularStatements
				+ ", numberOfCommunities=" + numberOfCommunities + ", organisation=" + organisation
				+ ", closed=" + closed + ", closedReasonID=" + closedReasonID + ", communities="
				+ communities + ", rescheduleLoans=" + rescheduleLoans + ", originalVillageBankID="
				+ originalVillageBankID + ", internetBankingRegistration="
				+ internetBankingRegistration + ", ibCommConsent=" + ibCommConsent
				+ ", webHookQueueNotAllowed=" + webHookQueueNotAllowed + ", addressCode="
				+ addressCode + ", customerNumber=" + customerNumber + "]";
	}

}

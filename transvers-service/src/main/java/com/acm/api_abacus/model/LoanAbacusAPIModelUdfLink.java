/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanAbacusAPIModelUdfLink} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelUdfLink implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6161530367944568976L;

	/** The udf link ID. */
	private int udfLinkID;

	/** The survey ID. */
	private int surveyID;

	/** The customer ID. */
	private int customerID;

	/** The cu account ID. */
	private int cuAccountID;

	/** The product ID. */
	private int productID;

	/** The cu insurance policy ID. */
	private int cuInsurancePolicyID;

	/** The receipt no. */
	private int receiptNo;

	/** The branch ID. */
	private int branchID;

	/** The interaction ID. */
	private int interactionID;

	/** The cu loan review ID. */
	private int cuLoanReviewID;

	/** The udf field ID. */
	private int udfFieldID;

	/** The value. */
	private String value;

	/** The user defined field list ID. */
	private int userDefinedFieldListID;

	/** The order. */
	private int order;

	/** The mask. */
	private String mask;

	/** The mandatory. */
	private boolean mandatory;

	/** The unique field. */
	private boolean uniqueField;

	/** The validate active UDF. */
	private boolean validateActiveUDF;

	/** The udf type. */
	private int udfType;

	/** The field name. */
	private String fieldName;

	/** The field currency. */
	private int fieldCurrency;

	/** The date value. */
	private Date dateValue;

	/** The customer types. */
	private int customerTypes;

	/** The description. */
	private String description;

	/** The field description. */
	private String fieldDescription;

	/**
	 * Instantiates a new loan abacus API model udf link.
	 */
	public LoanAbacusAPIModelUdfLink() {

		/*
		 * Empty
		 */
	}

	/**
	 * Gets the udf link ID.
	 *
	 * @return the udfLinkID
	 */
	public int getUdfLinkID() {

		return udfLinkID;
	}

	/**
	 * Sets the udf link ID.
	 *
	 * @param udfLinkID the udfLinkID to set
	 */
	public void setUdfLinkID(int udfLinkID) {

		this.udfLinkID = udfLinkID;
	}

	/**
	 * Gets the survey ID.
	 *
	 * @return the surveyID
	 */
	public int getSurveyID() {

		return surveyID;
	}

	/**
	 * Sets the survey ID.
	 *
	 * @param surveyID the surveyID to set
	 */
	public void setSurveyID(int surveyID) {

		this.surveyID = surveyID;
	}

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
	 * Gets the cu account ID.
	 *
	 * @return the cuAccountID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the cuAccountID to set
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the product ID.
	 *
	 * @return the productID
	 */
	public int getProductID() {

		return productID;
	}

	/**
	 * Sets the product ID.
	 *
	 * @param productID the productID to set
	 */
	public void setProductID(int productID) {

		this.productID = productID;
	}

	/**
	 * Gets the cu insurance policy ID.
	 *
	 * @return the cuInsurancePolicyID
	 */
	public int getCuInsurancePolicyID() {

		return cuInsurancePolicyID;
	}

	/**
	 * Sets the cu insurance policy ID.
	 *
	 * @param cuInsurancePolicyID the cuInsurancePolicyID to set
	 */
	public void setCuInsurancePolicyID(int cuInsurancePolicyID) {

		this.cuInsurancePolicyID = cuInsurancePolicyID;
	}

	/**
	 * Gets the receipt no.
	 *
	 * @return the receiptNo
	 */
	public int getReceiptNo() {

		return receiptNo;
	}

	/**
	 * Sets the receipt no.
	 *
	 * @param receiptNo the receiptNo to set
	 */
	public void setReceiptNo(int receiptNo) {

		this.receiptNo = receiptNo;
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
	 * Gets the interaction ID.
	 *
	 * @return the interactionID
	 */
	public int getInteractionID() {

		return interactionID;
	}

	/**
	 * Sets the interaction ID.
	 *
	 * @param interactionID the interactionID to set
	 */
	public void setInteractionID(int interactionID) {

		this.interactionID = interactionID;
	}

	/**
	 * Gets the cu loan review ID.
	 *
	 * @return the cuLoanReviewID
	 */
	public int getCuLoanReviewID() {

		return cuLoanReviewID;
	}

	/**
	 * Sets the cu loan review ID.
	 *
	 * @param cuLoanReviewID the cuLoanReviewID to set
	 */
	public void setCuLoanReviewID(int cuLoanReviewID) {

		this.cuLoanReviewID = cuLoanReviewID;
	}

	/**
	 * Gets the udf field ID.
	 *
	 * @return the udfFieldID
	 */
	public int getUdfFieldID() {

		return udfFieldID;
	}

	/**
	 * Sets the udf field ID.
	 *
	 * @param udfFieldID the udfFieldID to set
	 */
	public void setUdfFieldID(int udfFieldID) {

		this.udfFieldID = udfFieldID;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(String value) {

		this.value = value;
	}

	/**
	 * Gets the user defined field list ID.
	 *
	 * @return the userDefinedFieldListID
	 */
	public int getUserDefinedFieldListID() {

		return userDefinedFieldListID;
	}

	/**
	 * Sets the user defined field list ID.
	 *
	 * @param userDefinedFieldListID the userDefinedFieldListID to set
	 */
	public void setUserDefinedFieldListID(int userDefinedFieldListID) {

		this.userDefinedFieldListID = userDefinedFieldListID;
	}

	/**
	 * Gets the order.
	 *
	 * @return the order
	 */
	public int getOrder() {

		return order;
	}

	/**
	 * Sets the order.
	 *
	 * @param order the order to set
	 */
	public void setOrder(int order) {

		this.order = order;
	}

	/**
	 * Gets the mask.
	 *
	 * @return the mask
	 */
	public String getMask() {

		return mask;
	}

	/**
	 * Sets the mask.
	 *
	 * @param mask the mask to set
	 */
	public void setMask(String mask) {

		this.mask = mask;
	}

	/**
	 * Checks if is mandatory.
	 *
	 * @return the mandatory
	 */
	public boolean isMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(boolean mandatory) {

		this.mandatory = mandatory;
	}

	/**
	 * Checks if is unique field.
	 *
	 * @return the uniqueField
	 */
	public boolean isUniqueField() {

		return uniqueField;
	}

	/**
	 * Sets the unique field.
	 *
	 * @param uniqueField the uniqueField to set
	 */
	public void setUniqueField(boolean uniqueField) {

		this.uniqueField = uniqueField;
	}

	/**
	 * Checks if is validate active UDF.
	 *
	 * @return the validateActiveUDF
	 */
	public boolean isValidateActiveUDF() {

		return validateActiveUDF;
	}

	/**
	 * Sets the validate active UDF.
	 *
	 * @param validateActiveUDF the validateActiveUDF to set
	 */
	public void setValidateActiveUDF(boolean validateActiveUDF) {

		this.validateActiveUDF = validateActiveUDF;
	}

	/**
	 * Gets the udf type.
	 *
	 * @return the udfType
	 */
	public int getUdfType() {

		return udfType;
	}

	/**
	 * Sets the udf type.
	 *
	 * @param udfType the udfType to set
	 */
	public void setUdfType(int udfType) {

		this.udfType = udfType;
	}

	/**
	 * Gets the field name.
	 *
	 * @return the fieldName
	 */
	public String getFieldName() {

		return fieldName;
	}

	/**
	 * Sets the field name.
	 *
	 * @param fieldName the fieldName to set
	 */
	public void setFieldName(String fieldName) {

		this.fieldName = fieldName;
	}

	/**
	 * Gets the field currency.
	 *
	 * @return the fieldCurrency
	 */
	public int getFieldCurrency() {

		return fieldCurrency;
	}

	/**
	 * Sets the field currency.
	 *
	 * @param fieldCurrency the fieldCurrency to set
	 */
	public void setFieldCurrency(int fieldCurrency) {

		this.fieldCurrency = fieldCurrency;
	}

	/**
	 * Gets the date value.
	 *
	 * @return the dateValue
	 */
	public Date getDateValue() {

		return dateValue;
	}

	/**
	 * Sets the date value.
	 *
	 * @param dateValue the dateValue to set
	 */
	public void setDateValue(Date dateValue) {

		this.dateValue = dateValue;
	}

	/**
	 * Gets the customer types.
	 *
	 * @return the customerTypes
	 */
	public int getCustomerTypes() {

		return customerTypes;
	}

	/**
	 * Sets the customer types.
	 *
	 * @param customerTypes the customerTypes to set
	 */
	public void setCustomerTypes(int customerTypes) {

		this.customerTypes = customerTypes;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the field description.
	 *
	 * @return the fieldDescription
	 */
	public String getFieldDescription() {

		return fieldDescription;
	}

	/**
	 * Sets the field description.
	 *
	 * @param fieldDescription the fieldDescription to set
	 */
	public void setFieldDescription(String fieldDescription) {

		this.fieldDescription = fieldDescription;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelUdfLink [udfLinkID=" + udfLinkID + ", surveyID=" + surveyID
				+ ", customerID=" + customerID + ", cuAccountID=" + cuAccountID + ", productID="
				+ productID + ", cuInsurancePolicyID=" + cuInsurancePolicyID + ", receiptNo="
				+ receiptNo + ", branchID=" + branchID + ", interactionID=" + interactionID
				+ ", cuLoanReviewID=" + cuLoanReviewID + ", udfFieldID=" + udfFieldID + ", value="
				+ value + ", userDefinedFieldListID=" + userDefinedFieldListID + ", order=" + order
				+ ", mask=" + mask + ", mandatory=" + mandatory + ", uniqueField=" + uniqueField
				+ ", validateActiveUDF=" + validateActiveUDF + ", udfType=" + udfType
				+ ", fieldName=" + fieldName + ", fieldCurrency=" + fieldCurrency + ", dateValue="
				+ dateValue + ", customerTypes=" + customerTypes + ", description=" + description
				+ ", fieldDescription=" + fieldDescription + "]";
	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link UDFLinksGroupeFieldsModelDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.8
 */
public class UDFLinksGroupeFieldsModelDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2625262864746002917L;

	/** The udf field ID. */
	private Long udfFieldID;

	/** The value. */
	private String value;

	/** The field value id. */
	private Long fieldValueId;

	/** The user defined field list ID. */
	private Long userDefinedFieldListID;

	/** The order. */
	private Integer order;

	/** The mask. */
	private String mask;

	/** The mandatory. */
	private Boolean mandatory;

	/** The unique field. */
	private Boolean uniqueField;

	/** The validate active UDF. */
	private Boolean validateActiveUDF;

	/** The udf type. */
	private Integer udfType;

	/** The field name. */
	private String fieldName;

	/** The field currency. */
	private String fieldCurrency;

	/** The date value. */
	private Date dateValue;

	/** The customer types. */
	private Integer customerTypes;

	/** The id abacus UDF link. */
	private Long idAbacusUDFLink;

	/** The surveys id. */
	private Long surveysId;

	/** The id (ID of {@link UserDefinedFieldsLinksDTO}). */
	private Long id;

	/** The index group. */
	private Long indexGroup;

	/**
	 * Instantiates a new UDF groupe fields model.
	 */
	public UDFLinksGroupeFieldsModelDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new UDF groupe fields model.
	 *
	 * @param udfFieldID the udf field ID
	 * @param value the value
	 * @param userDefinedFieldListID the user defined field list ID
	 * @param order the order
	 * @param mask the mask
	 * @param mandatory the mandatory
	 * @param uniqueField the unique field
	 * @param validateActiveUDF the validate active UDF
	 * @param udfType the udf type
	 * @param fieldName the field name
	 * @param fieldCurrency the field currency
	 * @param dateValue the date value
	 * @param customerTypes the customer types
	 */
	public UDFLinksGroupeFieldsModelDTO(Long udfFieldID, String value, Long userDefinedFieldListID,
			Integer order, String mask, Boolean mandatory, Boolean uniqueField,
			Boolean validateActiveUDF, Integer udfType, String fieldName, String fieldCurrency,
			Date dateValue, Integer customerTypes) {

		this.udfFieldID = udfFieldID;
		this.value = value;
		this.userDefinedFieldListID = userDefinedFieldListID;
		this.order = order;
		this.mask = mask;
		this.mandatory = mandatory;
		this.uniqueField = uniqueField;
		this.validateActiveUDF = validateActiveUDF;
		this.udfType = udfType;
		this.fieldName = fieldName;
		this.fieldCurrency = fieldCurrency;
		this.dateValue = dateValue;
		this.customerTypes = customerTypes;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UDFGroupeFieldsModel [udfFieldID=" + udfFieldID + ", value=" + value
				+ ", userDefinedFieldListID=" + userDefinedFieldListID + ", order=" + order
				+ ", mask=" + mask + ", mandatory=" + mandatory + ", uniqueField=" + uniqueField
				+ ", validateActiveUDF=" + validateActiveUDF + ", udfType=" + udfType
				+ ", fieldName=" + fieldName + ", fieldCurrency=" + fieldCurrency + ", dateValue="
				+ dateValue + ", customerTypes=" + customerTypes + "]";
	}

	/**
	 * Gets the udf field ID.
	 *
	 * @return the udfFieldID
	 */
	public Long getUdfFieldID() {

		return udfFieldID;
	}

	/**
	 * Sets the udf field ID.
	 *
	 * @param udfFieldID the udfFieldID to set
	 */
	public void setUdfFieldID(Long udfFieldID) {

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
	public Long getUserDefinedFieldListID() {

		return userDefinedFieldListID;
	}

	/**
	 * Sets the user defined field list ID.
	 *
	 * @param userDefinedFieldListID the userDefinedFieldListID to set
	 */
	public void setUserDefinedFieldListID(Long userDefinedFieldListID) {

		this.userDefinedFieldListID = userDefinedFieldListID;
	}

	/**
	 * Gets the order.
	 *
	 * @return the order
	 */
	public Integer getOrder() {

		return order;
	}

	/**
	 * Sets the order.
	 *
	 * @param order the order to set
	 */
	public void setOrder(Integer order) {

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
	 * Gets the mandatory.
	 *
	 * @return the mandatory
	 */
	public Boolean getMandatory() {

		return mandatory;
	}

	/**
	 * Sets the mandatory.
	 *
	 * @param mandatory the mandatory to set
	 */
	public void setMandatory(Boolean mandatory) {

		this.mandatory = mandatory;
	}

	/**
	 * Gets the unique field.
	 *
	 * @return the uniqueField
	 */
	public Boolean getUniqueField() {

		return uniqueField;
	}

	/**
	 * Sets the unique field.
	 *
	 * @param uniqueField the uniqueField to set
	 */
	public void setUniqueField(Boolean uniqueField) {

		this.uniqueField = uniqueField;
	}

	/**
	 * Gets the validate active UDF.
	 *
	 * @return the validateActiveUDF
	 */
	public Boolean getValidateActiveUDF() {

		return validateActiveUDF;
	}

	/**
	 * Sets the validate active UDF.
	 *
	 * @param validateActiveUDF the validateActiveUDF to set
	 */
	public void setValidateActiveUDF(Boolean validateActiveUDF) {

		this.validateActiveUDF = validateActiveUDF;
	}

	/**
	 * Gets the udf type.
	 *
	 * @return the udfType
	 */
	public Integer getUdfType() {

		return udfType;
	}

	/**
	 * Sets the udf type.
	 *
	 * @param udfType the udfType to set
	 */
	public void setUdfType(Integer udfType) {

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
	public String getFieldCurrency() {

		return fieldCurrency;
	}

	/**
	 * Sets the field currency.
	 *
	 * @param fieldCurrency the fieldCurrency to set
	 */
	public void setFieldCurrency(String fieldCurrency) {

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
	public Integer getCustomerTypes() {

		return customerTypes;
	}

	/**
	 * Sets the customer types.
	 *
	 * @param customerTypes the customerTypes to set
	 */
	public void setCustomerTypes(Integer customerTypes) {

		this.customerTypes = customerTypes;
	}

	/**
	 * Gets the id abacus UDF link.
	 *
	 * @return the idAbacusUDFLink
	 */
	public Long getIdAbacusUDFLink() {

		return idAbacusUDFLink;
	}

	/**
	 * Sets the id abacus UDF link.
	 *
	 * @param idAbacusUDFLink the idAbacusUDFLink to set
	 */
	public void setIdAbacusUDFLink(Long idAbacusUDFLink) {

		this.idAbacusUDFLink = idAbacusUDFLink;
	}

	/**
	 * Gets the surveys id.
	 *
	 * @return the surveysId
	 */
	public Long getSurveysId() {

		return surveysId;
	}

	/**
	 * Sets the surveys id.
	 *
	 * @param surveysId the surveysId to set
	 */
	public void setSurveysId(Long surveysId) {

		this.surveysId = surveysId;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the field value id.
	 *
	 * @return the fieldValueId
	 */
	public Long getFieldValueId() {

		return fieldValueId;
	}

	/**
	 * Sets the field value id.
	 *
	 * @param fieldValueId the fieldValueId to set
	 */
	public void setFieldValueId(Long fieldValueId) {

		this.fieldValueId = fieldValueId;
	}

	/**
	 * Gets the index group.
	 *
	 * @return the index group
	 */
	public Long getIndexGroup() {

		return indexGroup;
	}

	/**
	 * Sets the index group.
	 *
	 * @param indexGroup the new index group
	 */
	public void setIndexGroup(Long indexGroup) {

		this.indexGroup = indexGroup;
	}

}

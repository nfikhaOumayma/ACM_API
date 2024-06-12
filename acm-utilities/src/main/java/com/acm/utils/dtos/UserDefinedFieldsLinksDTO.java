/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import org.dozer.Mapping;

import com.acm.utils.models.UserDefinedFieldsLinks;

/**
 * {@link UserDefinedFieldsLinks} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UserDefinedFieldsLinksDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7182699692644391704L;

	/** The id. */
	private Long id;

	/** The user defined fields. */
	@Mapping("userDefinedFields")
	private UserDefinedFieldsDTO userDefinedFieldsDTO;

	/** The loan id. */
	private Long loanId;

	/** The cu account id. */
	private Long cuAccountId;

	/** The customer id. */
	private Long customerId;

	/** The udf list value id. */
	private Long udfListValueId;

	/** The field value. */
	private String fieldValue;

	/** The field value id. */
	private Long fieldValueId;

	/** The id abacus UDF link. */
	private Long idAbacusUDFLink;

	/** The token. */
	private String token;

	/** The surveys id. */
	private Long surveysId;

	/** The index group. */
	private Long indexGroup;

	/** The cutomer type. */
	private String cutomerType;

	/** The product id. */
	private String productId;

	/** The enable data. */
	private Boolean enableData;

	/** The ib loan id. */
	private Long ibLoanId;

	/** The ib customer id. */
	private Long ibCustomerId;

	/** The category. */
	private String category;

	/** The element id. */
	private Long elementId;

	/** The udf group ids. */
	private List<Long> udfGroupIds;

	/**
	 * Instantiates a new user defined fields links.
	 */
	public UserDefinedFieldsLinksDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined fields links DTO.
	 *
	 * @param userDefinedFieldsDTO the user defined fields DTO
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @param udfListValueId the udf list value id
	 * @param fieldValue the field value
	 */
	public UserDefinedFieldsLinksDTO(UserDefinedFieldsDTO userDefinedFieldsDTO, Long loanId,
			Long customerId, Long udfListValueId, String fieldValue) {

		this.userDefinedFieldsDTO = userDefinedFieldsDTO;
		this.loanId = loanId;
		this.customerId = customerId;
		this.udfListValueId = udfListValueId;
		this.fieldValue = fieldValue;
	}

	/**
	 * Instantiates a new user defined fields links DTO (USED IN ABACUS API SERVICE).
	 *
	 * @param userDefinedFieldsDTO the user defined fields DTO
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @param fieldValue the field value
	 * @param idAbacusUDFLink the id abacus UDF link
	 * @param surveysId the surveys id
	 * @param udfListValueId the udf list value id
	 */
	public UserDefinedFieldsLinksDTO(UserDefinedFieldsDTO userDefinedFieldsDTO, Long loanId,
			Long customerId, String fieldValue, Long idAbacusUDFLink, Long surveysId,
			Long udfListValueId) {

		this.userDefinedFieldsDTO = userDefinedFieldsDTO;
		this.loanId = loanId;
		this.customerId = customerId;
		this.fieldValue = fieldValue;
		this.idAbacusUDFLink = idAbacusUDFLink;
		this.surveysId = surveysId;
		this.udfListValueId = udfListValueId;
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
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Gets the customer id.
	 *
	 * @return the customerId
	 */
	public Long getCustomerId() {

		return customerId;
	}

	/**
	 * Sets the customer id.
	 *
	 * @param customerId the customerId to set
	 */
	public void setCustomerId(Long customerId) {

		this.customerId = customerId;
	}

	/**
	 * Gets the udf list value id.
	 *
	 * @return the udfListValueId
	 */
	public Long getUdfListValueId() {

		return udfListValueId;
	}

	/**
	 * Sets the udf list value id.
	 *
	 * @param udfListValueId the udfListValueId to set
	 */
	public void setUdfListValueId(Long udfListValueId) {

		this.udfListValueId = udfListValueId;
	}

	/**
	 * Gets the field value.
	 *
	 * @return the fieldValue
	 */
	public String getFieldValue() {

		return fieldValue;
	}

	/**
	 * Sets the field value.
	 *
	 * @param fieldValue the fieldValue to set
	 */
	public void setFieldValue(String fieldValue) {

		this.fieldValue = fieldValue;
	}

	/**
	 * Gets the user defined fields DTO.
	 *
	 * @return the userDefinedFieldsDTO
	 */
	public UserDefinedFieldsDTO getUserDefinedFieldsDTO() {

		return userDefinedFieldsDTO;
	}

	/**
	 * Sets the user defined fields DTO.
	 *
	 * @param userDefinedFieldsDTO the userDefinedFieldsDTO to set
	 */
	public void setUserDefinedFieldsDTO(UserDefinedFieldsDTO userDefinedFieldsDTO) {

		this.userDefinedFieldsDTO = userDefinedFieldsDTO;
	}

	/**
	 * Gets the cu account id.
	 *
	 * @return the cuAccountId
	 */
	public Long getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the cuAccountId to set
	 */
	public void setCuAccountId(Long cuAccountId) {

		this.cuAccountId = cuAccountId;
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
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
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
	 * Gets the cutomer type.
	 *
	 * @return the cutomerType
	 */
	public String getCutomerType() {

		return cutomerType;
	}

	/**
	 * Sets the cutomer type.
	 *
	 * @param cutomerType the cutomerType to set
	 */
	public void setCutomerType(String cutomerType) {

		this.cutomerType = cutomerType;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the productId
	 */
	public String getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the productId to set
	 */
	public void setProductId(String productId) {

		this.productId = productId;
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

	/**
	 * Gets the enable data.
	 *
	 * @return the enable data
	 */
	public Boolean getEnableData() {

		return enableData;
	}

	/**
	 * Sets the enable data.
	 *
	 * @param enableData the new enable data
	 */
	public void setEnableData(Boolean enableData) {

		this.enableData = enableData;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldsLinksDTO [id=" + id + ", userDefinedFieldsDTO="
				+ userDefinedFieldsDTO + ", loanId=" + loanId + ", customerId=" + customerId
				+ ", udfListValueId=" + udfListValueId + ", fieldValue=" + fieldValue
				+ ", idAbacusUDFLink=" + idAbacusUDFLink + ", surveysId=" + surveysId + "]";
	}

	/**
	 * Gets the ib loan id.
	 *
	 * @return the ib loan id
	 */
	public Long getIbLoanId() {

		return ibLoanId;
	}

	/**
	 * Sets the ib loan id.
	 *
	 * @param ibLoanId the new ib loan id
	 */
	public void setIbLoanId(Long ibLoanId) {

		this.ibLoanId = ibLoanId;
	}

	/**
	 * Gets the ib customer id.
	 *
	 * @return the ib customer id
	 */
	public Long getIbCustomerId() {

		return ibCustomerId;
	}

	/**
	 * Sets the ib customer id.
	 *
	 * @param ibCustomerId the new ib customer id
	 */
	public void setIbCustomerId(Long ibCustomerId) {

		this.ibCustomerId = ibCustomerId;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the element id.
	 *
	 * @return the element id
	 */
	public Long getElementId() {

		return elementId;
	}

	/**
	 * Sets the element id.
	 *
	 * @param elementId the new element id
	 */
	public void setElementId(Long elementId) {

		this.elementId = elementId;
	}

	/**
	 * Gets the udf group ids.
	 *
	 * @return the udf group ids
	 */
	public List<Long> getUdfGroupIds() {

		return udfGroupIds;
	}

	/**
	 * Sets the udf group ids.
	 *
	 * @param udfGroupIds the new udf group ids
	 */
	public void setUdfGroupIds(List<Long> udfGroupIds) {

		this.udfGroupIds = udfGroupIds;
	}

}

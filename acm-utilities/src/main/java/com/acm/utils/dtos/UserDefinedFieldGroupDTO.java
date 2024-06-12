/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.enums.SettingUDFCustomerType;
import com.acm.utils.models.UserDefinedFieldGroup;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link UserDefinedFieldGroup} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class UserDefinedFieldGroupDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2964608697593757554L;

	/** The id. */
	private Long id;

	/** The id UD group abacus. */
	private Long idUDGroupAbacus;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The loan id. */
	private Long loanId;

	/**
	 * The UDF customer type: - 0 : groupe crédit - 1 : Individuel - 4 : Organisation - 8 : Groupe -
	 * - 5 : Individuel & organisation - 9 : Individuel & groupe - 12 : organisation et groupe -----
	 * - 13 : Tous les types.
	 */
	private Integer customerType;

	/** The customer type label. */
	private String customerTypeLabel;

	/** The customer id. */
	private Long customerId;

	/** The product id. */
	private String productId;

	/** The enabled. */
	private Boolean enabled;

	/** The mondatory. */
	private Boolean mondatory;

	/** The UDF category. */
	private String category;

	/**
	 * Instantiates a new user defined field group.
	 */
	public UserDefinedFieldGroupDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined field group DTO.
	 *
	 * @param idUDGroupAbacus the id UD group abacus
	 */
	public UserDefinedFieldGroupDTO(Long idUDGroupAbacus) {

		this.idUDGroupAbacus = idUDGroupAbacus;
	}

	/**
	 * Instantiates a new user defined field group DTO.
	 *
	 * @param idUDGroupAbacus the id UD group abacus
	 * @param code the code
	 * @param description the description
	 * @param loanId the loan id
	 * @param customerType the customer type
	 * @param customerId the customer id
	 * @param productId the product id
	 * @param enabled the enabled
	 * @param mondatory the mondatory
	 */
	public UserDefinedFieldGroupDTO(Long idUDGroupAbacus, String code, String description,
			Long loanId, Integer customerType, Long customerId, String productId, Boolean enabled,
			Boolean mondatory) {

		this.idUDGroupAbacus = idUDGroupAbacus;
		this.code = code;
		this.description = description;
		this.loanId = loanId;
		this.customerType = customerType;
		this.customerId = customerId;
		this.productId = productId;
		this.enabled = enabled;
		this.mondatory = mondatory;
	}

	/**
	 * Instantiates a new user defined field group DTO.
	 *
	 * @param id the id
	 * @param idUDGroupAbacus the id UD group abacus
	 * @param code the code
	 * @param description the description
	 * @param loanId the loan id
	 * @param customerType the customer type
	 * @param customerTypeLabel the customer type label
	 * @param customerId the customer id
	 * @param productId the product id
	 * @param enabled the enabled
	 */
	public UserDefinedFieldGroupDTO(Long id, Long idUDGroupAbacus, String code, String description,
			Long loanId, Integer customerType, String customerTypeLabel, Long customerId,
			String productId, Boolean enabled) {

		this.id = id;
		this.idUDGroupAbacus = idUDGroupAbacus;
		this.code = code;
		this.description = description;
		this.loanId = loanId;
		this.customerType = customerType;
		this.customerTypeLabel = customerTypeLabel;
		this.customerId = customerId;
		this.productId = productId;
		this.enabled = enabled;
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
	 * Gets the id UD group abacus.
	 *
	 * @return the idUDGroupAbacus
	 */
	public Long getIdUDGroupAbacus() {

		return idUDGroupAbacus;
	}

	/**
	 * Sets the id UD group abacus.
	 *
	 * @param idUDGroupAbacus the idUDGroupAbacus to set
	 */
	public void setIdUDGroupAbacus(Long idUDGroupAbacus) {

		this.idUDGroupAbacus = idUDGroupAbacus;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
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
	 * Gets the customer type.
	 *
	 * @return the customerType
	 */
	public Integer getCustomerType() {

		return customerType;
	}

	/**
	 * Sets the customer type.
	 *
	 * @param customerType the customerType to set
	 */
	public void setCustomerType(Integer customerType) {

		this.customerType = customerType;
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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the customer type label.
	 *
	 * @return the customerTypeLabel
	 */
	public String getCustomerTypeLabel() {

		if (ACMValidationUtils.isNullOrEmpty(customerType)) {
			return customerTypeLabel;
		}
		return SettingUDFCustomerType.typeName(customerType);
	}

	/**
	 * Sets the customer type label.
	 *
	 * @param customerTypeLabel the customerTypeLabel to set
	 */
	public void setCustomerTypeLabel(String customerTypeLabel) {

		this.customerTypeLabel = customerTypeLabel;
	}

	/**
	 * Gets the mondatory.
	 *
	 * @return the mondatory
	 */
	public Boolean getMondatory() {

		return mondatory;
	}

	/**
	 * Sets the mondatory.
	 *
	 * @param mondatory the mondatory to set
	 */
	public void setMondatory(Boolean mondatory) {

		this.mondatory = mondatory;
	}

	/**
	 * Gets the category.
	 * 
	 * @author nrmila
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 * 
	 * @author nrmila
	 * @param category The new category to set.
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldGroupDTO [id=" + id + ", idUDGroupAbacus=" + idUDGroupAbacus
				+ ", code=" + code + ", description=" + description + ", loanId=" + loanId
				+ ", customerType=" + customerType + ", customerTypeLabel=" + customerTypeLabel
				+ ", customerId=" + customerId + ", productId=" + productId + ", enabled=" + enabled
				+ "]";
	}

}

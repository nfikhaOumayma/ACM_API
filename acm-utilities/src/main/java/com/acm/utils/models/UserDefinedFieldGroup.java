/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import com.acm.utils.audit.AuditTrailListener;

/**
 * {@link UserDefinedFieldGroup} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_UDF_GROUPE")
@EntityListeners(AuditTrailListener.class)
public class UserDefinedFieldGroup extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7381535403676724685L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_UDF_GROUPE", unique = true, nullable = false)
	private Long id;

	/** The id UD group abacus. */
	@Column(name = "ID_ABACUS_UDF_GROUPE")
	private Long idUDGroupAbacus;

	/** The code. */
	@Column(name = "CODE")
	private String code;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The loan id. */
	@Column(name = "LOAN")
	private Long loanId;

	/**
	 * The UDF customer type: - 0 : groupe crédit - 1 : Individuel - 4 : Organisation - 8 : Groupe -
	 * - 5 : Individuel & organisation - 9 : Individuel & groupe - 12 : organisation et groupe -----
	 * - 13 : Tous les types.
	 */
	@Column(name = "CUSTOMER_TYPE")
	private Integer customerType;

	/** The customer id. */
	@Column(name = "CUSTOMER")
	private Long customerId;

	/** The product id. */
	@Column(name = "PRODUCTID")
	private String productId;

	/** The product id. */
	@Column(name = "MONDATORY")
	private Boolean mondatory;

	/** The UDF category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The user defined fields. */
	@OneToMany(mappedBy = "userDefinedFieldGroup")
	@OrderBy("ordre")
	private Set<UserDefinedFields> userDefinedFields = new HashSet<>();

	/**
	 * Instantiates a new user defined field group.
	 */
	public UserDefinedFieldGroup() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new user defined field group.
	 *
	 * @param id the id
	 */
	public UserDefinedFieldGroup(Long id) {

		this.id = id;
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
	 * Gets the user defined fields.
	 *
	 * @return the userDefinedFields
	 */
	public Set<UserDefinedFields> getUserDefinedFields() {

		return userDefinedFields;
	}

	/**
	 * Sets the user defined fields.
	 *
	 * @param userDefinedFields the userDefinedFields to set
	 */
	public void setUserDefinedFields(Set<UserDefinedFields> userDefinedFields) {

		this.userDefinedFields = userDefinedFields;
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
	 * @return The category.
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

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link AcmLoanInstanceAcmGroupeApproval} class.
 *
 * @author mkhmissi
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_LOAN_INSTANCE_ACM_GROUPE_APPROVAL")
public class AcmLoanInstanceAcmGroupeApproval extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 669751172431379057L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "LOAN_INSTANCE_GROUPE_ID")
	private Long id;

	/** The loan instance. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ACM_LOAN_INSTANCE_ID")
	private LoanInstance loanInstance;

	/** The groupe. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ACM_GROUPE_ID")
	private Groupe groupe;

	/** The validation. */
	@Column(name = "ACM_LOAN_VALIDATION")
	private Boolean validation;

	/** The owner name. */
	@Column(name = "ACM_OWNER_NAME")
	private String ownerName;

	/** The owner ID. */
	@Column(name = "ACM_OWNER")
	private String owner;

	/** The groupe name. */
	@Column(name = "ACM_GROUPE_NAME")
	private String groupeName;

	/** The groupe code. */
	@Column(name = "ACM_GROUPE_CODE")
	private String groupeCode;

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the groupe name.
	 *
	 * @return the groupe name
	 */
	public String getGroupeName() {

		return groupeName;
	}

	/**
	 * Sets the groupe name.
	 *
	 * @param groupeName the new groupe name
	 */
	public void setGroupeName(String groupeName) {

		this.groupeName = groupeName;
	}

	/**
	 * Instantiates a new acm loan instance groupe association.
	 */
	public AcmLoanInstanceAcmGroupeApproval() {

		super();
	}

	/**
	 * Gets the groupe code.
	 *
	 * @return the groupe code
	 */
	public String getGroupeCode() {

		return groupeCode;
	}

	/**
	 * Sets the groupe code.
	 *
	 * @param groupeCode the new groupe code
	 */
	public void setGroupeCode(String groupeCode) {

		this.groupeCode = groupeCode;
	}

	/**
	 * Gets the loan instance.
	 *
	 * @return the loan instance
	 */
	public LoanInstance getLoanInstance() {

		return loanInstance;
	}

	/**
	 * Sets the loan instance.
	 *
	 * @param loanInstance the new loan instance
	 */
	public void setLoanInstance(LoanInstance loanInstance) {

		this.loanInstance = loanInstance;
	}

	/**
	 * Gets the groupe.
	 *
	 * @return the groupe
	 */
	public Groupe getGroupe() {

		return groupe;
	}

	/**
	 * Sets the groupe.
	 *
	 * @param groupe the new groupe
	 */
	public void setGroupe(Groupe groupe) {

		this.groupe = groupe;
	}

	/**
	 * Gets the validation.
	 *
	 * @return the validation
	 */
	public Boolean getValidation() {

		return validation;
	}

	/**
	 * Sets the validation.
	 *
	 * @param validation the new validation
	 */
	public void setValidation(Boolean validation) {

		this.validation = validation;
	}

	/**
	 * Gets the owner name.
	 *
	 * @return the owner name
	 */
	public String getOwnerName() {

		return ownerName;
	}

	/**
	 * Sets the owner name.
	 *
	 * @param ownerName the new owner name
	 */
	public void setOwnerName(String ownerName) {

		this.ownerName = ownerName;
	}

	/**
	 * Gets the owner ID.
	 *
	 * @return the owner ID
	 */
	public String getOwner() {

		return owner;
	}

	/**
	 * Sets the owner ID.
	 *
	 * @param owner the new owner ID
	 */
	public void setOwner(String owner) {

		this.owner = owner;
	}

	/**
	 * Instantiates a new acm loan instance groupe association.
	 *
	 * @param loanInstance the loan instance
	 * @param groupe the groupe
	 * @param validation the validation
	 * @param ownerName the owner name
	 * @param owner the owner ID
	 * @param groupeName the groupe name
	 */
	public AcmLoanInstanceAcmGroupeApproval(LoanInstance loanInstance, Groupe groupe,
			Boolean validation, String ownerName, String owner, String groupeName) {

		super();
		this.loanInstance = loanInstance;
		this.groupe = groupe;
		this.validation = validation;
		this.ownerName = ownerName;
		this.owner = owner;
		this.groupeName = groupeName;
	}

	/**
	 * Instantiates a new acm loan instance groupe association.
	 *
	 * @param id the id
	 * @param loanInstance the loan instance
	 * @param groupe the groupe
	 * @param validation the validation
	 * @param ownerName the owner name
	 * @param owner the owner ID
	 * @param groupeName the groupe name
	 * @param groupeCode the groupe code
	 */
	public AcmLoanInstanceAcmGroupeApproval(Long id, LoanInstance loanInstance, Groupe groupe,
			Boolean validation, String ownerName, String owner, String groupeName,
			String groupeCode) {

		super();
		this.id = id;
		this.loanInstance = loanInstance;
		this.groupe = groupe;
		this.validation = validation;
		this.ownerName = ownerName;
		this.owner = owner;
		this.groupeName = groupeName;
		this.groupeCode = groupeCode;
	}

	/**
	 * Instantiates a new acm loan instance groupe association.
	 *
	 * @param loanInstance the loan instance
	 * @param groupe the groupe
	 * @param validation the validation
	 * @param ownerName the owner name
	 * @param owner the owner ID
	 * @param groupeName the groupe name
	 * @param groupeCode the groupe code
	 */
	public AcmLoanInstanceAcmGroupeApproval(LoanInstance loanInstance, Groupe groupe,
			Boolean validation, String ownerName, String owner, String groupeName,
			String groupeCode) {

		super();
		this.loanInstance = loanInstance;
		this.groupe = groupe;
		this.validation = validation;
		this.ownerName = ownerName;
		this.owner = owner;
		this.groupeName = groupeName;
		this.groupeCode = groupeCode;
	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class AcmLoanInstanceGroupe_AssociationDTO.
 */
public class AcmLoanInstanceAcmGroupeApprovalDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7364601083781432515L;

	/** The id. */
	private Long id;

	/** The loan instance. */
	private LoanInstanceDTO loanInstance;

	/** The groupe. */
	private GroupeDTO groupe;

	/** The validation. */
	private Boolean validation;

	/** The owner name. */
	private String ownerName;

	/** The owner ID. */
	private String owner;

	/** The groupe name. */
	private String groupeName;

	/** The groupe code. */
	private String groupeCode;

	/**
	 * Instantiates a new acm loan instance groupe association DTO.
	 */
	public AcmLoanInstanceAcmGroupeApprovalDTO() {

		super();
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
	 * Gets the loan instance.
	 *
	 * @return the loan instance
	 */
	public LoanInstanceDTO getLoanInstance() {

		return loanInstance;
	}

	/**
	 * Sets the loan instance.
	 *
	 * @param loanInstance the new loan instance
	 */
	public void setLoanInstance(LoanInstanceDTO loanInstance) {

		this.loanInstance = loanInstance;
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
	 * Gets the serialversionuid.
	 *
	 * @return the serialversionuid
	 */
	public static long getSerialversionuid() {

		return serialVersionUID;
	}

	/**
	 * Gets the groupe.
	 *
	 * @return the groupe
	 */
	public GroupeDTO getGroupe() {

		return groupe;
	}

	/**
	 * Sets the groupe.
	 *
	 * @param groupe the new groupe
	 */
	public void setGroupe(GroupeDTO groupe) {

		this.groupe = groupe;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmLoanInstanceGroupe_AssociationDTO [id=" + id + ", loanInstance=" + loanInstance
				+ ", groupe=" + groupe + ", validation=" + validation + ", ownerName=" + ownerName
				+ ", owner=" + owner + ", groupeName=" + groupeName + "]";
	}

	/**
	 * Instantiates a new acm loan instance groupe association DTO.
	 *
	 * @param loanInstance the loan instance
	 * @param groupe the groupe
	 * @param validation the validation
	 * @param ownerName the owner name
	 * @param owner the owner ID
	 * @param groupeName the groupe name
	 */
	public AcmLoanInstanceAcmGroupeApprovalDTO(LoanInstanceDTO loanInstance, GroupeDTO groupe,
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
	 * Instantiates a new acm loan instance groupe association DTO.
	 *
	 * @param id the id
	 * @param loanInstance the loan instance
	 * @param groupe the groupe
	 * @param validation the validation
	 * @param ownerName the owner name
	 * @param owner the owner ID
	 */
	public AcmLoanInstanceAcmGroupeApprovalDTO(Long id, LoanInstanceDTO loanInstance, GroupeDTO groupe,
			Boolean validation, String ownerName, String owner) {

		super();
		this.id = id;
		this.loanInstance = loanInstance;
		this.groupe = groupe;
		this.validation = validation;
		this.ownerName = ownerName;
		this.owner = owner;
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
	 * Instantiates a new acm loan instance groupe association DTO.
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
	public AcmLoanInstanceAcmGroupeApprovalDTO(Long id, LoanInstanceDTO loanInstance, GroupeDTO groupe,
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

}

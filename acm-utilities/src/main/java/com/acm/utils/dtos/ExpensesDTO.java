/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link ExpensesDTO} class.
 *
 * @author Ines Dridi
 * @since 1.1.3
 */
public class ExpensesDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2288960535541516785L;

	/** The id. */
	private Long id;

	/** The id expenses type. */
	private Long idExpensesType;

	/** The id branch. */
	private Long idBranch;

	/** The branch description. */
	private String branchDescription;

	/** The description. */
	private String description;

	/** The statut. */
	private Integer statut; // '0' :new , '1' : accepted , '-1': rejected

	/** The owner. */
	private String owner;

	/** The teller. */
	private String teller;

	/** The balance. */
	private Long balance;

	/** The expenses amount. */
	private Long expensesAmount;

	/** The apply date. */
	private Date applyDate;

	/** The expenses type libelle. */
	private String expensesTypeLibelle;

	/** The teller name. */
	private String tellerName;

	/** The owner name. */
	private String ownerName;

	/** The note. */
	private String note;

	/**
	 * Instantiates a new expenses.
	 */
	public ExpensesDTO() {

	}

	/**
	 * Instantiates a new expenses DTO.
	 *
	 * @param statut the statut
	 */
	public ExpensesDTO(Integer statut) {

		this.statut = statut;
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
	 * Gets the id expenses type.
	 *
	 * @return the id expenses type
	 */
	public Long getIdExpensesType() {

		return idExpensesType;
	}

	/**
	 * Sets the id expenses type.
	 *
	 * @param idExpensesType the new id expenses type
	 */
	public void setIdExpensesType(Long idExpensesType) {

		this.idExpensesType = idExpensesType;
	}

	/**
	 * Gets the id branch.
	 *
	 * @return the id branch
	 */
	public Long getIdBranch() {

		return idBranch;
	}

	/**
	 * Sets the id branch.
	 *
	 * @param idBranch the new id branch
	 */
	public void setIdBranch(Long idBranch) {

		this.idBranch = idBranch;
	}

	/**
	 * Gets the branch description.
	 *
	 * @return the branch description
	 */
	public String getBranchDescription() {

		return branchDescription;
	}

	/**
	 * Sets the branch description.
	 *
	 * @param branchDescription the new branch description
	 */
	public void setBranchDescription(String branchDescription) {

		this.branchDescription = branchDescription;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the statut.
	 *
	 * @return the statut
	 */
	public Integer getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the new statut
	 */
	public void setStatut(Integer statut) {

		this.statut = statut;
	}

	/**
	 * Gets the owner.
	 *
	 * @return the owner
	 */
	public String getOwner() {

		return owner;
	}

	/**
	 * Sets the owner.
	 *
	 * @param owner the new owner
	 */
	public void setOwner(String owner) {

		this.owner = owner;
	}

	/**
	 * Gets the teller.
	 *
	 * @return the teller
	 */
	public String getTeller() {

		return teller;
	}

	/**
	 * Sets the teller.
	 *
	 * @param teller the new teller
	 */
	public void setTeller(String teller) {

		this.teller = teller;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public Long getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the new balance
	 */
	public void setBalance(Long balance) {

		this.balance = balance;
	}

	/**
	 * Gets the expenses amount.
	 *
	 * @return the expenses amount
	 */
	public Long getExpensesAmount() {

		return expensesAmount;
	}

	/**
	 * Sets the expenses amount.
	 *
	 * @param expensesAmount the new expenses amount
	 */
	public void setExpensesAmount(Long expensesAmount) {

		this.expensesAmount = expensesAmount;
	}

	/**
	 * Gets the apply date.
	 *
	 * @return the apply date
	 */
	public Date getApplyDate() {

		return applyDate;
	}

	/**
	 * Sets the apply date.
	 *
	 * @param applyDate the new apply date
	 */
	public void setApplyDate(Date applyDate) {

		this.applyDate = applyDate;
	}

	/**
	 * Gets the expenses type libelle.
	 *
	 * @return the expenses type libelle
	 */
	public String getExpensesTypeLibelle() {

		return expensesTypeLibelle;
	}

	/**
	 * Sets the expenses type libelle.
	 *
	 * @param expensesTypeLibelle the new expenses type libelle
	 */
	public void setExpensesTypeLibelle(String expensesTypeLibelle) {

		this.expensesTypeLibelle = expensesTypeLibelle;
	}

	/**
	 * Gets the teller name.
	 *
	 * @return the teller name
	 */
	public String getTellerName() {

		return tellerName;
	}

	/**
	 * Sets the teller name.
	 *
	 * @param tellerName the new teller name
	 */
	public void setTellerName(String tellerName) {

		this.tellerName = tellerName;
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
	 * Gets the note.
	 *
	 * @return the note
	 */
	public String getNote() {

		return note;
	}

	/**
	 * Sets the note.
	 *
	 * @param note the new note
	 */
	public void setNote(String note) {

		this.note = note;
	}

}

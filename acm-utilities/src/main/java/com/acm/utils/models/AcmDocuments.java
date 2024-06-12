/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.Date;

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
 * {@link AcmDocuments} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Entity
@Table(name = "ACM_DOCUMENTS")
public class AcmDocuments extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5768558922098556847L;

	/** The id document. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_DOCUMENTS", unique = true, nullable = false)
	private Long idDocument;

	/** The loan. */
	@Column(name = "ID_LOAN")
	private Long loanId;

	/** The setting document type. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_DOC_TYPE")
	private SettingDocumentType settingDocumentType;

	/** The id customer. */
	@Column(name = "ID_CUSTOMER")
	private Long idCustomer;

	/** The id document GED. */
	@Column(name = "ID_DOCUMENT_GED")
	private String idDocumentGED;

	/** The titre. */
	@Column(name = "TITRE", nullable = false)
	private String titre;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The auteur. */
	@Column(name = "AUTEUR")
	private String auteur;

	/** The date creation. */
	@Column(name = "DATE_CREATION")
	private Date dateCreation;

	/** The account number extern. */
	@Column(name = "ACCOUNT_NUMBER_EXTERN")
	private String accountNumberExtern;

	/** The customer name. */
	@Column(name = "CUSTOMER_NAME")
	private String customerName;

	/** The mandatory. */
	@Column(name = "MANDATORY")
	private Boolean mandatory;

	/** The document index. */
	@Column(name = "DOCUMENT_INDEX")
	private Integer documentIndex;

	/** The document name. */
	@Column(name = "DOCUMENT_NAME")
	private String name;

	/** The expenses id. */
	@Column(name = "ID_EXPENSES")
	private Long expensesId;

	/** The collection instance id. */
	@Column(name = "ID_COLLECTION_INSTANCE")
	private Long collectionInstanceId;

	/** The collection instance id. */
	@Column(name = "ID_ITEM_INSTANCE")
	private Long itemInstanceId;

	/** The id ib document. */
	@Column(name = "ID_IB_DOCUMENT")
	private Long idIbDocument;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The element id. */
	@Column(name = "ELEMENT_ID")
	private Long elementId;

	/**
	 * Instantiates a new acm documents.
	 */
	public AcmDocuments() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id document.
	 *
	 * @return the idDocument
	 */
	public Long getIdDocument() {

		return idDocument;
	}

	/**
	 * Sets the id document.
	 *
	 * @param idDocument the idDocument to set
	 */
	public void setIdDocument(Long idDocument) {

		this.idDocument = idDocument;
	}

	/**
	 * Gets the id document GED.
	 *
	 * @return the idDocumentGED
	 */
	public String getIdDocumentGED() {

		return idDocumentGED;
	}

	/**
	 * Sets the id document GED.
	 *
	 * @param idDocumentGED the idDocumentGED to set
	 */
	public void setIdDocumentGED(String idDocumentGED) {

		this.idDocumentGED = idDocumentGED;
	}

	/**
	 * Gets the titre.
	 *
	 * @return the titre
	 */
	public String getTitre() {

		return titre;
	}

	/**
	 * Sets the titre.
	 *
	 * @param titre the titre to set
	 */
	public void setTitre(String titre) {

		this.titre = titre;
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
	 * Gets the auteur.
	 *
	 * @return the auteur
	 */
	public String getAuteur() {

		return auteur;
	}

	/**
	 * Sets the auteur.
	 *
	 * @param auteur the auteur to set
	 */
	public void setAuteur(String auteur) {

		this.auteur = auteur;
	}

	/**
	 * Gets the date creation.
	 *
	 * @return the dateCreation
	 */
	public Date getDateCreation() {

		return dateCreation;
	}

	/**
	 * Sets the date creation.
	 *
	 * @param dateCreation the dateCreation to set
	 */
	public void setDateCreation(Date dateCreation) {

		this.dateCreation = dateCreation;
	}

	/**
	 * Gets the setting document type.
	 *
	 * @return the settingDocumentType
	 */
	public SettingDocumentType getSettingDocumentType() {

		return settingDocumentType;
	}

	/**
	 * Sets the setting document type.
	 *
	 * @param settingDocumentType the settingDocumentType to set
	 */
	public void setSettingDocumentType(SettingDocumentType settingDocumentType) {

		this.settingDocumentType = settingDocumentType;
	}

	/**
	 * Gets the id customer.
	 *
	 * @return the idCustomer
	 */
	public Long getIdCustomer() {

		return idCustomer;
	}

	/**
	 * Sets the id customer.
	 *
	 * @param idCustomer the idCustomer to set
	 */
	public void setIdCustomer(Long idCustomer) {

		this.idCustomer = idCustomer;
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
	 * Gets the account number extern.
	 *
	 * @return the accountNumberExtern
	 */
	public String getAccountNumberExtern() {

		return accountNumberExtern;
	}

	/**
	 * Sets the account number extern.
	 *
	 * @param accountNumberExtern the accountNumberExtern to set
	 */
	public void setAccountNumberExtern(String accountNumberExtern) {

		this.accountNumberExtern = accountNumberExtern;
	}

	/**
	 * Gets the customer name.
	 *
	 * @return the customerName
	 */
	public String getCustomerName() {

		return customerName;
	}

	/**
	 * Sets the customer name.
	 *
	 * @param customerName the customerName to set
	 */
	public void setCustomerName(String customerName) {

		this.customerName = customerName;
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
	 * Gets the document index.
	 * 
	 * @return the documentIndex
	 */
	public Integer getDocumentIndex() {

		return documentIndex;
	}

	/**
	 * Sets the document index.
	 * 
	 * @param documentIndex the documentIndex to set
	 */
	public void setDocumentIndex(Integer documentIndex) {

		this.documentIndex = documentIndex;
	}

	/**
	 * Gets the document name.
	 * 
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the document name.
	 * 
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the expenses id.
	 *
	 * @return the expenses id
	 */
	public Long getExpensesId() {

		return expensesId;
	}

	/**
	 * Sets the expenses id.
	 *
	 * @param expensesId the new expenses id
	 */
	public void setExpensesId(Long expensesId) {

		this.expensesId = expensesId;
	}

	/**
	 * Gets the collection instance id.
	 *
	 * @return the collection instance id
	 */
	public Long getCollectionInstanceId() {

		return collectionInstanceId;
	}

	/**
	 * Sets the collection instance id.
	 *
	 * @param collectionInstanceId the new collection instance id
	 */
	public void setCollectionInstanceId(Long collectionInstanceId) {

		this.collectionInstanceId = collectionInstanceId;
	}

	/**
	 * Gets the id ib document.
	 *
	 * @return the id ib document
	 */
	public Long getIdIbDocument() {

		return idIbDocument;
	}

	/**
	 * Sets the id ib document.
	 *
	 * @param idIbDocument the new id ib document
	 */
	public void setIdIbDocument(Long idIbDocument) {

		this.idIbDocument = idIbDocument;
	}

	/**
	 * Gets the item instance id.
	 *
	 * @return the item instance id
	 */
	public Long getItemInstanceId() {

		return itemInstanceId;
	}

	/**
	 * Sets the item instance id.
	 *
	 * @param itemInstanceId the new item instance id
	 */
	public void setItemInstanceId(Long itemInstanceId) {

		this.itemInstanceId = itemInstanceId;
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

}

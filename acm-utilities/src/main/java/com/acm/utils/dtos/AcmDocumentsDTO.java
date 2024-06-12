/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import org.dozer.Mapping;
import org.springframework.web.multipart.MultipartFile;

import com.acm.utils.models.AcmDocuments;

/**
 * {@link AcmDocuments} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
public class AcmDocumentsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -8278165452559394739L;

	/** The id document. */
	private Long idDocument;

	/** The loan DTO. */
	private Long loanId;

	/** The setting document type DTO. */
	@Mapping("settingDocumentType")
	private SettingDocumentTypeDTO settingDocumentTypeDTO;

	/** The id customer. */
	private Long idCustomer;

	/** The id document GED. */
	private String idDocumentGED;

	/** The titre. */
	private String titre;

	/** The description. */
	private String description;

	/** The auteur. */
	private String auteur;

	/** The date creation. */
	private Date dateCreation;

	/** The enabled. */
	private Boolean enabled;

	/** The insertBy. */
	private String insertBy;

	/** The account number extern. */
	private String accountNumberExtern;

	/** The customer name. */
	private String customerName;

	/** The mandatory. */
	private Boolean mandatory;

	/** The document index. */
	private Integer documentIndex;

	/** The document name. */
	private String name;

	/** The process loan documents : used ONLY in loan document process. */
	private Boolean processLoanDocuments;

	/** The customer number. */
	private String customerNumber;

	/** The customer identity. */
	private String customerIdentity;

	/** The telephone1. */
	private String telephone1;

	/** The expenses id. */
	private Long expensesId;

	/** The document size. */
	private Integer documentSize;

	/** The multipart file. */
	private MultipartFile multipartFile;

	/** The collection instance id. */
	private Long collectionInstanceId;

	/** The report name. */
	private String reportName;

	/** The updated by. */
	private String updatedBy;

	/** The document file. */
	private byte[] documentFile;

	/** The ib document id. */
	private Long idIbDocument;

	/** The item instance id. */
	private Long itemInstanceId;

	/** The work flow step id. */
	private Long workFlowStepId;

	/** The item instance ids. */
	private List<Long> itemInstanceIds;

	/** The category. */
	private String category;

	/** The element id. */
	private Long elementId;

	/**
	 * Instantiates a new acm documents DTO.
	 */
	public AcmDocumentsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm documents DTO.
	 *
	 * @param idDocument the id document
	 */
	public AcmDocumentsDTO(Long idDocument) {

		this.idDocument = idDocument;
	}

	/**
	 * Instantiates a new acm documents DTO : USED in CHECK METHOD : isExist().
	 *
	 * @param idDocument the id document
	 * @param idDocumentGED the id document GED
	 */
	public AcmDocumentsDTO(Long idDocument, String idDocumentGED) {

		this.idDocument = idDocument;
		this.idDocumentGED = idDocumentGED;
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
	 * Gets the setting document type DTO.
	 *
	 * @return the settingDocumentTypeDTO
	 */
	public SettingDocumentTypeDTO getSettingDocumentTypeDTO() {

		return settingDocumentTypeDTO;
	}

	/**
	 * Sets the setting document type DTO.
	 *
	 * @param settingDocumentTypeDTO the settingDocumentTypeDTO to set
	 */
	public void setSettingDocumentTypeDTO(SettingDocumentTypeDTO settingDocumentTypeDTO) {

		this.settingDocumentTypeDTO = settingDocumentTypeDTO;
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
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
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
	 * Gets the process loan documents.
	 *
	 * @return the processLoanDocuments
	 */
	public Boolean getProcessLoanDocuments() {

		return processLoanDocuments;
	}

	/**
	 * Sets the process loan documents.
	 *
	 * @param processLoanDocuments the processLoanDocuments to set
	 */
	public void setProcessLoanDocuments(Boolean processLoanDocuments) {

		this.processLoanDocuments = processLoanDocuments;
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

	/**
	 * Gets the customer identity.
	 *
	 * @return the customer identity
	 */
	public String getCustomerIdentity() {

		return customerIdentity;
	}

	/**
	 * Sets the customer identity.
	 *
	 * @param customerIdentity the new customer identity
	 */
	public void setCustomerIdentity(String customerIdentity) {

		this.customerIdentity = customerIdentity;
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
	 * Gets the report name.
	 *
	 * @return the report name
	 */
	public String getReportName() {

		return reportName;
	}

	/**
	 * Sets the report name.
	 *
	 * @param reportName the new report name
	 */
	public void setReportName(String reportName) {

		this.reportName = reportName;
	}

	/**
	 * Gets the document size.
	 *
	 * @return the documentSize
	 */
	public Integer getDocumentSize() {

		return documentSize;
	}

	/**
	 * Sets the document size.
	 *
	 * @param documentSize the documentSize to set
	 */
	public void setDocumentSize(Integer documentSize) {

		this.documentSize = documentSize;
	}

	/**
	 * Gets the multipart file.
	 *
	 * @return the multipartFile
	 */
	public MultipartFile getMultipartFile() {

		return multipartFile;
	}

	/**
	 * Sets the multipart file.
	 *
	 * @param multipartFile the multipartFile to set
	 */
	public void setMultipartFile(MultipartFile multipartFile) {

		this.multipartFile = multipartFile;
	}

	/**
	 * Gets the telephone 1.
	 *
	 * @return the telephone 1
	 */
	public String getTelephone1() {

		return telephone1;
	}

	/**
	 * Sets the telephone 1.
	 *
	 * @param telephone1 the new telephone 1
	 */
	public void setTelephone1(String telephone1) {

		this.telephone1 = telephone1;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updatedBy
	 */
	public String getUpdatedBy() {

		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the updatedBy to set
	 */
	public void setUpdatedBy(String updatedBy) {

		this.updatedBy = updatedBy;
	}

	/**
	 * Gets the document file.
	 *
	 * @return the document file
	 */
	public byte[] getDocumentFile() {

		return documentFile;
	}

	/**
	 * Sets the document file.
	 *
	 * @param documentFile the new document file
	 */
	public void setDocumentFile(byte[] documentFile) {

		this.documentFile = documentFile;
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
	 * Gets the work flow step id.
	 *
	 * @return the work flow step id
	 */
	public Long getWorkFlowStepId() {

		return workFlowStepId;
	}

	/**
	 * Sets the work flow step id.
	 *
	 * @param workFlowStepId the new work flow step id
	 */
	public void setWorkFlowStepId(Long workFlowStepId) {

		this.workFlowStepId = workFlowStepId;
	}

	/**
	 * Gets the item instance ids.
	 *
	 * @return the item instance ids
	 */
	public List<Long> getItemInstanceIds() {

		return itemInstanceIds;
	}

	/**
	 * Sets the item instance ids.
	 *
	 * @param itemInstanceIds the new item instance ids
	 */
	public void setItemInstanceIds(List<Long> itemInstanceIds) {

		this.itemInstanceIds = itemInstanceIds;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmDocumentsDTO [idDocument=" + idDocument + ", loanId=" + loanId
				+ ", settingDocumentTypeDTO=" + settingDocumentTypeDTO + ", idCustomer="
				+ idCustomer + ", idDocumentGED=" + idDocumentGED + ", titre=" + titre
				+ ", description=" + description + ", auteur=" + auteur + ", dateCreation="
				+ dateCreation + ", enabled=" + enabled + ", insertBy=" + insertBy
				+ ", accountNumberExtern=" + accountNumberExtern + ", customerName=" + customerName
				+ ", mandatory=" + mandatory + ", documentIndex=" + documentIndex + ", name=" + name
				+ ", processLoanDocuments=" + processLoanDocuments + ", customerNumber="
				+ customerNumber + ", customerIdentity=" + customerIdentity + ", telephone1="
				+ telephone1 + ", expensesId=" + expensesId + ", documentSize=" + documentSize
				+ ", multipartFile=" + multipartFile + ", collectionInstanceId="
				+ collectionInstanceId + ", reportName=" + reportName + ", updatedBy=" + updatedBy
				+ "]";
	}

}

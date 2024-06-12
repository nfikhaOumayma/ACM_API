/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.File;
import java.io.Serializable;
import java.util.List;

/**
 * {@link GedParameterDTO} class contains parameters related to Ged files.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public class GedParameterDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3728533434569765278L;

	/** The files to upload. */
	private List<File> filesToUpload;

	/** The path. */
	private String path;

	/** The tags. */
	private List<String> tags;

	/** The site. */
	private String site;

	/** The all. */
	private Boolean all;

	/** The id document. */
	private Long idDocument;

	/** The loan. */
	private Long loanId;

	/** The id customer. */
	private Long idCustomer;

	/** The loan number. */
	private String loanNumber;

	/** The customer number. */
	private String customerNumber;

	/** The id document ged. */
	private String idDocumentGed;

	/** The acm documents DTO. */
	private AcmDocumentsDTO acmDocumentsDTO;

	/**
	 * Instantiates a new ged parameter DTO.
	 */
	public GedParameterDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new ged parameter DTO.
	 *
	 * @param filesToUpload the files to upload
	 * @param path the path to save in
	 * @param tags the tags to add
	 */
	public GedParameterDTO(List<File> filesToUpload, String path, List<String> tags) {

		this.filesToUpload = filesToUpload;
		this.path = path;
		this.tags = tags;
	}

	/**
	 * Instantiates a new ged parameter DTO.
	 *
	 * @param filesToUpload the files to upload
	 * @param path the path
	 * @param tags the tags
	 * @param site the site
	 */
	public GedParameterDTO(List<File> filesToUpload, String path, List<String> tags, String site) {

		this.filesToUpload = filesToUpload;
		this.path = path;
		this.tags = tags;
		this.setSite(site);
	}

	/**
	 * Gets the files to upload.
	 *
	 * @return the filesToUpload
	 */
	public List<File> getFilesToUpload() {

		return filesToUpload;
	}

	/**
	 * Sets the files to upload.
	 *
	 * @param filesToUpload the filesToUpload to set
	 */
	public void setFilesToUpload(List<File> filesToUpload) {

		this.filesToUpload = filesToUpload;
	}

	/**
	 * Gets the tags.
	 *
	 * @return the tags
	 */
	public List<String> getTags() {

		return tags;
	}

	/**
	 * Sets the tags.
	 *
	 * @param tags the tags to set
	 */
	public void setTags(List<String> tags) {

		this.tags = tags;
	}

	/**
	 * Gets the path.
	 *
	 * @return the path
	 */
	public String getPath() {

		return path;
	}

	/**
	 * Sets the path.
	 *
	 * @param path the path to set
	 */
	public void setPath(String path) {

		this.path = path;
	}

	/**
	 * Gets the site.
	 *
	 * @return the site
	 */
	public String getSite() {

		return site;
	}

	/**
	 * Sets the site.
	 *
	 * @param site the site to set
	 */
	public void setSite(String site) {

		this.site = site;
	}

	/**
	 * Gets the all.
	 *
	 * @return the all
	 */
	public Boolean getAll() {

		return all;
	}

	/**
	 * Sets the all.
	 *
	 * @param all the all to set
	 */
	public void setAll(Boolean all) {

		this.all = all;
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
	 * Gets the loan number.
	 *
	 * @return the loanNumber
	 */
	public String getLoanNumber() {

		return loanNumber;
	}

	/**
	 * Sets the loan number.
	 *
	 * @param loanNumber the loanNumber to set
	 */
	public void setLoanNumber(String loanNumber) {

		this.loanNumber = loanNumber;
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
	 * Gets the id document ged.
	 *
	 * @return the idDocumentGed
	 */
	public String getIdDocumentGed() {

		return idDocumentGed;
	}

	/**
	 * Sets the id document ged.
	 *
	 * @param idDocumentGed the idDocumentGed to set
	 */
	public void setIdDocumentGed(String idDocumentGed) {

		this.idDocumentGed = idDocumentGed;
	}

	/**
	 * Gets the acm documents DTO.
	 *
	 * @return the acmDocumentsDTO
	 */
	public AcmDocumentsDTO getAcmDocumentsDTO() {

		return acmDocumentsDTO;
	}

	/**
	 * Sets the acm documents DTO.
	 *
	 * @param acmDocumentsDTO the acmDocumentsDTO to set
	 */
	public void setAcmDocumentsDTO(AcmDocumentsDTO acmDocumentsDTO) {

		this.acmDocumentsDTO = acmDocumentsDTO;
	}
}

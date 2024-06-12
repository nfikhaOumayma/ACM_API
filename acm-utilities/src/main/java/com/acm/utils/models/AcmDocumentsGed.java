/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link AcmDocumentsGed} class.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
@Entity
@Table(name = "ACM_DOCUMENTS_GED")
public class AcmDocumentsGed extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1942971036732827625L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_DOCUMENTS_GED", unique = true, nullable = false)
	private Long id;

	/** The id document. */
	@Column(name = "ID_ACM_DOCUMENTS", nullable = false)
	private Long idDocument;

	/** The loan. */
	@Column(name = "ID_LOAN")
	private Long loanId;

	/** The id customer. */
	@Column(name = "ID_CUSTOMER", nullable = false)
	private Long idCustomer;

	/** The document ged. */
	@Column(name = "DOCUMENT_GED", nullable = false)
	private byte[] documentGed;

	/**
	 * Instantiates a new acm documents ged.
	 */
	public AcmDocumentsGed() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm documents ged.
	 *
	 * @param idDocument the id document
	 * @param loanId the loan id
	 * @param idCustomer the id customer
	 * @param documentGed the document ged
	 */
	public AcmDocumentsGed(Long idDocument, Long loanId, Long idCustomer, byte[] documentGed) {

		this.idDocument = idDocument;
		this.loanId = loanId;
		this.idCustomer = idCustomer;
		this.documentGed = documentGed;
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
	 * Gets the document ged.
	 *
	 * @return the documentGed
	 */
	public byte[] getDocumentGed() {

		return documentGed;
	}

	/**
	 * Sets the document ged.
	 *
	 * @param documentGed the documentGed to set
	 */
	public void setDocumentGed(byte[] documentGed) {

		this.documentGed = documentGed;
	}

}

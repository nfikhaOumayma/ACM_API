/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.models.AcmDocumentsGed;

/**
 * {@link AcmDocumentsGed} class.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
public class AcmDocumentsGedDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6423202051263602993L;

	/** The id. */
	private Long id;

	/** The id document. */
	private Long idDocument;

	/** The loan. */
	private Long loanId;

	/** The id customer. */
	private Long idCustomer;

	/** The document ged. */
	private byte[] documentGed;

	/**
	 * Instantiates a new acm documents ged DTO.
	 */
	public AcmDocumentsGedDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new acm documents ged DTO.
	 *
	 * @param idDocument the id document
	 * @param loanId the loan id
	 * @param idCustomer the id customer
	 * @param documentGed the document ged
	 */
	public AcmDocumentsGedDTO(Long idDocument, Long loanId, Long idCustomer, byte[] documentGed) {

		this.idDocument = idDocument;
		this.loanId = loanId;
		this.idCustomer = idCustomer;
		this.documentGed = documentGed;
	}

	/**
	 * Instantiates a new acm documents ged DTO.
	 *
	 * @param idDocument the id document
	 * @param loanId the loan id
	 * @param idCustomer the id customer
	 */
	public AcmDocumentsGedDTO(Long idDocument, Long loanId, Long idCustomer) {

		this.idDocument = idDocument;
		this.loanId = loanId;
		this.idCustomer = idCustomer;
	}

	/**
	 * Instantiates a new acm documents ged DTO : USED in CHECK METHOD : isExist().
	 *
	 * @param idDocument the id document
	 */
	public AcmDocumentsGedDTO(Long idDocument) {

		this.idDocument = idDocument;
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

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmDocumentsGedDTO [id=" + id + ", idDocument=" + idDocument + ", loanId=" + loanId
				+ ", idCustomer=" + idCustomer + "]";
	}
}

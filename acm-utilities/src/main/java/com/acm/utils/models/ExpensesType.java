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
 * The persistent class for the ACM_EXPENSES_TYPES table. {@link ExpensesType} class.
 * 
 * @author YesserSomai
 * @since 1.1.3
 */
@Entity
@Table(name = "ACM_EXPENSES_TYPES")
public class ExpensesType extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4382555459185662773L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_EXPENSES_TYPES", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", unique = true)
	private String code;

	/** The libel. */
	@Column(name = "LIBEL")
	private String libel;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The document ID. */
	@Column(name = "DOCUMENT_ID")
	private Long documentID;

	/** The document libel. */
	@Column(name = "DOCUMENT_LIBEL")
	private String documentLibel;

	/**
	 * Instantiates a new expenses type.
	 */
	public ExpensesType() {

		/*
		 * EMPTY
		 */
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
	 * @param code the new code
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the libel.
	 *
	 * @return the libel
	 */
	public String getLibel() {

		return libel;
	}

	/**
	 * Sets the libel.
	 *
	 * @param libel the new libel
	 */
	public void setLibel(String libel) {

		this.libel = libel;
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
	 * Gets the document ID.
	 *
	 * @return the document ID
	 */
	public Long getDocumentID() {

		return documentID;
	}

	/**
	 * Sets the document ID.
	 *
	 * @param documentID the new document ID
	 */
	public void setDocumentID(Long documentID) {

		this.documentID = documentID;
	}

	/**
	 * Gets the document libel.
	 *
	 * @return the document libel
	 */
	public String getDocumentLibel() {

		return documentLibel;
	}

	/**
	 * Sets the document libel.
	 *
	 * @param documentLibel the new document libel
	 */
	public void setDocumentLibel(String documentLibel) {

		this.documentLibel = documentLibel;
	}

}

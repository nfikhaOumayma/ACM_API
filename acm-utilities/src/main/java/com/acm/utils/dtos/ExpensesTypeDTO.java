/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The class {@link ExpensesTypeDTO}.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
public class ExpensesTypeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7251844129913763839L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The libel. */
	private String libel;

	/** The description. */
	private String description;

	/** The enabled. */
	private Boolean enabled;

	/** The document ID. */
	private Long documentID;

	/** The document libel. */
	private String documentLibel;

	/**
	 * Instantiates a new expenses type DTO.
	 */
	public ExpensesTypeDTO() {

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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
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

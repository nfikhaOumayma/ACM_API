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
 * {@link SettingMotifRejets} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_SETTING_MOTIFS_REJET")
public class SettingMotifRejets extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_MOTIFS_REJET", unique = true, nullable = false)
	private Long id;

	/** The categorie. REJECT / REVIEW / AUTRE */
	@Column(name = "CATEGORIE", nullable = false)
	private String categorie;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The code external. */
	@Column(name = "CODE_EXTERNAL")
	private Integer codeExternal;

	/**
	 * Instantiates a new setting motif rejets.
	 */
	public SettingMotifRejets() {

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
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the categorie.
	 *
	 * @return the categorie
	 */
	public String getCategorie() {

		return categorie;
	}

	/**
	 * Sets the categorie.
	 *
	 * @param categorie the categorie to set
	 */
	public void setCategorie(String categorie) {

		this.categorie = categorie;
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
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the libelle.
	 *
	 * @return the libelle
	 */
	public String getLibelle() {

		return libelle;
	}

	/**
	 * Sets the libelle.
	 *
	 * @param libelle the libelle to set
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
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
	 * Gets the code external.
	 *
	 * @return the codeExternal
	 */
	public Integer getCodeExternal() {

		return codeExternal;
	}

	/**
	 * Sets the code external.
	 *
	 * @param codeExternal the codeExternal to set
	 */
	public void setCodeExternal(Integer codeExternal) {

		this.codeExternal = codeExternal;
	}

}

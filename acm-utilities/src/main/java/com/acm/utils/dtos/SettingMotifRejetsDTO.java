/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingMotifRejetsDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public class SettingMotifRejetsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5586197244024675290L;

	/** The id. */
	private Long id;

	/** The categorie. REJET / CORRECTIFS / AUTRE */
	private String categorie;

	/** The code. */
	private String code;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The code external. */
	private Integer codeExternal;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new setting motif rejets DTO.
	 */
	public SettingMotifRejetsDTO() {

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

}

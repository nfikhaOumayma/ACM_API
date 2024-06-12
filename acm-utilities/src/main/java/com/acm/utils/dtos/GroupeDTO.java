/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * {@link GroupeDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class GroupeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2586348174477111385L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The user profile ID extern. */
	private Long userProfileIDExtern;

	/** The enabled. */
	private Boolean enabled;

	/** The user DT os. */
	private List<UserDTO> userDTOs = new ArrayList<>();

	/**
	 * Instantiates a new groupe DTO.
	 */
	public GroupeDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new groupe DTO.
	 *
	 * @param code the code
	 */
	public GroupeDTO(String code) {

		this.code = code;
	}

	/**
	 * Instantiates a new groupe DTO.
	 *
	 * @param code the code
	 * @param libelle the libelle
	 * @param description the description
	 * @param enabled the enabled
	 */
	public GroupeDTO(String code, String libelle, String description, Boolean enabled) {

		this.code = code;
		this.libelle = libelle;
		this.description = description;
		this.enabled = enabled;
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
	 * Gets the user profile ID extern.
	 *
	 * @return the userProfileIDExtern
	 */
	public Long getUserProfileIDExtern() {

		return userProfileIDExtern;
	}

	/**
	 * Sets the user profile ID extern.
	 *
	 * @param userProfileIDExtern the userProfileIDExtern to set
	 */
	public void setUserProfileIDExtern(Long userProfileIDExtern) {

		this.userProfileIDExtern = userProfileIDExtern;
	}

	/**
	 * Gets the user DT os.
	 *
	 * @return the userDTOs
	 */
	public List<UserDTO> getUserDTOs() {

		return userDTOs;
	}

	/**
	 * Sets the user DT os.
	 *
	 * @param userDTOs the userDTOs to set
	 */
	public void setUserDTOs(List<UserDTO> userDTOs) {

		this.userDTOs = userDTOs;
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

	@Override
	public String toString() {

		return "{ \"id\":" + id + ", \"code\":\"" + code + "\", \"libelle\":\"" + libelle
				+ "\", \"description\": \"" + description + "\", \"userProfileIDExtern\":"
				+ userProfileIDExtern + ", \"enabled\":\"" + enabled + "\"}";
	}

}

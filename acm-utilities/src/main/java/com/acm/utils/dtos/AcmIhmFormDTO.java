/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

import org.dozer.Mapping;

/**
 * {@link AcmIhmFormDTO} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public class AcmIhmFormDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4999913681577285759L;

	/** The id. */
	private Long id;

	/** The code page. */
	private String codePage;

	/** The description. */
	private String description;

	/** The habilitation IHM route. */
	private HabilitationIHMRouteDTO habilitationIHMRouteDTO;

	/** The acm ihm field DT os. */
	@Mapping("acmIhmFields")
	private List<AcmIhmFieldDTO> acmIhmFields;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new acm ihm form DTO.
	 */
	public AcmIhmFormDTO() {

		// EMPTY
	}

	/**
	 * Instantiates a new acm ihm form DTO.
	 *
	 * @param codePage the code page
	 */
	public AcmIhmFormDTO(String codePage) {

		this.codePage = codePage;
	}

	/** The need fields. */
	private Boolean needFields;

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
	 * Gets the code page.
	 *
	 * @return the code page
	 */
	public String getCodePage() {

		return codePage;
	}

	/**
	 * Sets the code page.
	 *
	 * @param codePage the new code page
	 */
	public void setCodePage(String codePage) {

		this.codePage = codePage;
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
	 * Gets the habilitation IHM route DTO.
	 *
	 * @return the habilitation IHM route DTO
	 */
	public HabilitationIHMRouteDTO getHabilitationIHMRouteDTO() {

		return habilitationIHMRouteDTO;
	}

	/**
	 * Sets the habilitation IHM route DTO.
	 *
	 * @param habilitationIHMRouteDTO the new habilitation IHM route DTO
	 */
	public void setHabilitationIHMRouteDTO(HabilitationIHMRouteDTO habilitationIHMRouteDTO) {

		this.habilitationIHMRouteDTO = habilitationIHMRouteDTO;
	}

	/**
	 * Gets the acm ihm fields.
	 *
	 * @return the acmIhmFields
	 */
	public List<AcmIhmFieldDTO> getAcmIhmFields() {

		return acmIhmFields;
	}

	/**
	 * Sets the acm ihm fields.
	 *
	 * @param acmIhmFields the acmIhmFields to set
	 */
	public void setAcmIhmFields(List<AcmIhmFieldDTO> acmIhmFields) {

		this.acmIhmFields = acmIhmFields;
	}

	/**
	 * Gets the need fields.
	 *
	 * @return the need fields
	 */
	public Boolean getNeedFields() {

		return needFields;
	}

	/**
	 * Sets the need fields.
	 *
	 * @param needFields the new need fields
	 */
	public void setNeedFields(Boolean needFields) {

		this.needFields = needFields;
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

}

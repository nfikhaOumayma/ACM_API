/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link AcmIhmFieldGroupeDTO } class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public class AcmIhmFieldGroupeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -739684024209225991L;

	/** The id. */
	@Mapping("id")
	private Long id;

	/** The habilitation. */
	private String habilitation;

	/** The acm ihm field. */
	@Mapping("acmIhmField")
	private AcmIhmFieldDTO acmIhmField;

	/** The group. */
	@Mapping("group")
	private GroupeDTO group;

	/**
	 * Instantiates a new acm ihm field groupe DTO.
	 */
	public AcmIhmFieldGroupeDTO() {

		// EMPTY
	}

	/**
	 * Gets the habilitation.
	 *
	 * @return the habilitation
	 */
	public String getHabilitation() {

		return habilitation;
	}

	/**
	 * Sets the habilitation.
	 *
	 * @param habilitation the new habilitation
	 */
	public void setHabilitation(String habilitation) {

		this.habilitation = habilitation;
	}

	/**
	 * Gets the acm ihm field.
	 *
	 * @return the acm ihm field
	 */
	public AcmIhmFieldDTO getAcmIhmField() {

		return acmIhmField;
	}

	/**
	 * Sets the acm ihm field.
	 *
	 * @param acmIhmField the new acm ihm field
	 */
	public void setAcmIhmField(AcmIhmFieldDTO acmIhmField) {

		this.acmIhmField = acmIhmField;
	}

	/**
	 * Gets the group.
	 *
	 * @return the group
	 */
	public GroupeDTO getGroup() {

		return group;
	}

	/**
	 * Sets the group.
	 *
	 * @param group the new group
	 */
	public void setGroup(GroupeDTO group) {

		this.group = group;
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
}

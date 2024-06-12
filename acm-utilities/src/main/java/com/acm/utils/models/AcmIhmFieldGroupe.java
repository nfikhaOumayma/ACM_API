/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * {@link AcmIhmFieldGroupe} class.
 *
 * @author LamloumManel
 * @since 1.0.14
 */
@Entity
@Table(name = "ACM_IHM_FIELD_GROUPE")
@NamedQuery(name = "AcmIhmFieldGroupe.findAll", query = "SELECT l FROM AcmIhmFieldGroupe l")
public class AcmIhmFieldGroupe extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6284262772481798801L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The habilitation. */
	@Column(name = "HABILITATION", length = 256)
	private String habilitation;

	/** The acm ihm field. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_ACM_IHM_FIELD")
	private AcmIhmField acmIhmField;

	/** The group. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_GROUPE")
	private Groupe group;

	/**
	 * Instantiates a new acm ihm field groupe.
	 */
	public AcmIhmFieldGroupe() {

		// Empty
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
	public AcmIhmField getAcmIhmField() {

		return acmIhmField;
	}

	/**
	 * Sets the acm ihm field.
	 *
	 * @param acmIhmField the new acm ihm field
	 */
	public void setAcmIhmField(AcmIhmField acmIhmField) {

		this.acmIhmField = acmIhmField;
	}

	/**
	 * Gets the group.
	 *
	 * @return the group
	 */
	public Groupe getGroup() {

		return group;
	}

	/**
	 * Sets the group.
	 *
	 * @param group the new group
	 */
	public void setGroup(Groupe group) {

		this.group = group;
	}

}

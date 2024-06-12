/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link AcmIhmForm} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Entity
@Table(name = "ACM_IHM_FORM")
@NamedQuery(name = "AcmIhmForm.findAll", query = "SELECT l FROM AcmIhmForm l")
public class AcmIhmForm extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3584271092892887158L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The code page. */
	@Column(name = "CODE_PAGE", length = 256)
	private String codePage;

	/** The description. */
	@Column(name = "DESCRIPTION", length = 256)
	private String description;

	/** The habilitation IHM route. */
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ID_ACM_HABILITATION_IHM_ROUTE")
	private HabilitationIHMRoute habilitationIHMRoute;

	/** The acm ihm fields. */
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "acmIhmForm")
	private Set<AcmIhmField> acmIhmFields = new HashSet<>();

	/**
	 * Instantiates a new acm ihm form.
	 */
	public AcmIhmForm() {

		// Empty
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
	 * Gets the habilitation IHM route.
	 *
	 * @return the habilitation IHM route
	 */
	public HabilitationIHMRoute getHabilitationIHMRoute() {

		return habilitationIHMRoute;
	}

	/**
	 * Sets the habilitation IHM route.
	 *
	 * @param habilitationIHMRoute the new habilitation IHM route
	 */
	public void setHabilitationIHMRoute(HabilitationIHMRoute habilitationIHMRoute) {

		this.habilitationIHMRoute = habilitationIHMRoute;
	}

	/**
	 * Gets the acm ihm fields.
	 *
	 * @return the acm ihm fields
	 */
	public Set<AcmIhmField> getAcmIhmFields() {

		return acmIhmFields;
	}

	/**
	 * Sets the acm ihm fields.
	 *
	 * @param acmIhmFields the new acm ihm fields
	 */
	public void setAcmIhmFields(Set<AcmIhmField> acmIhmFields) {

		this.acmIhmFields = acmIhmFields;
	}

}

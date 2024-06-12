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
import javax.persistence.ManyToMany;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * {@link AcmIhmValidator} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Entity
@Table(name = "ACM_IHM_VALIDATOR")
@NamedQuery(name = "AcmIhmValidator.findAll", query = "SELECT l FROM AcmIhmValidator l")
public class AcmIhmValidator extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2319317568801136341L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The code validator. */
	@Column(name = "CODE_VALIDATOR", length = 256)
	private String codeValidator;

	/** The type validator. */
	@Column(name = "TYPE_VALIDATOR", length = 256)
	private String typeValidator;

	/** The parameter. */
	@Column(name = "PARAMETER", length = 256)
	private String parameter;

	/** The validators. */
	@ManyToMany(fetch = FetchType.LAZY, mappedBy = "validators")
	private Set<AcmIhmField> fields = new HashSet<>();

	/**
	 * Instantiates a new acm ihm validator.
	 */
	public AcmIhmValidator() {

		// EMPTY
	}

	/**
	 * Instantiates a new acm ihm validator.
	 *
	 * @param id the id
	 */
	public AcmIhmValidator(Long id) {

		this.id = id;
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
	 * Gets the parameter.
	 *
	 * @return the parameter
	 */
	public String getParameter() {

		return parameter;
	}

	/**
	 * Sets the parameter.
	 *
	 * @param parameter the new parameter
	 */
	public void setParameter(String parameter) {

		this.parameter = parameter;
	}

	/**
	 * Gets the code validator.
	 *
	 * @return the code validator
	 */
	public String getCodeValidator() {

		return codeValidator;
	}

	/**
	 * Sets the code validator.
	 *
	 * @param codeValidator the new code validator
	 */
	public void setCodeValidator(String codeValidator) {

		this.codeValidator = codeValidator;
	}

	/**
	 * Gets the fields.
	 *
	 * @return the fields
	 */
	public Set<AcmIhmField> getFields() {

		return fields;
	}

	/**
	 * Sets the fields.
	 *
	 * @param fields the new fields
	 */
	public void setFields(Set<AcmIhmField> fields) {

		this.fields = fields;
	}

	/**
	 * Gets the type validator.
	 *
	 * @return the type validator
	 */
	public String getTypeValidator() {

		return typeValidator;
	}

	/**
	 * Sets the type validator.
	 *
	 * @param typeValidator the new type validator
	 */
	public void setTypeValidator(String typeValidator) {

		this.typeValidator = typeValidator;
	}

}

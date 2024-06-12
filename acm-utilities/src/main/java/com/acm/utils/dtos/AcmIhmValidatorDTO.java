/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link AcmIhmValidatorDTO} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public class AcmIhmValidatorDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2325477243914002366L;

	/** The id. */
	private Long id;

	/** The code validator. */
	private String codeValidator;

	/** The parameter. */
	private String parameter;

	/** The enabled. */
	private Boolean enabled;

	/** The type validator. */
	private String typeValidator;

	/**
	 * Instantiates a new acm ihm validator DTO.
	 */
	public AcmIhmValidatorDTO() {

		// EMPTY
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

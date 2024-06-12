/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link DeferredPeriodTypeDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class DeferredPeriodTypeDTO extends GenericDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -330299743426406396L;

	/** The deferred period type id. */
	private Long deferredPeriodTypeId;

	/** The code. */
	private String code;

	/** The enabled. */
	private Boolean enabled;

	/**
	 * Instantiates a new deferred period type DTO.
	 */
	public DeferredPeriodTypeDTO() {

	}

	/**
	 * Instantiates a new deferred period type DTO.
	 *
	 * @param deferredPeriodTypeId the deferred period type id
	 * @param code the code
	 */
	public DeferredPeriodTypeDTO(Long deferredPeriodTypeId, String code) {

		this.deferredPeriodTypeId = deferredPeriodTypeId;
		this.code = code;
	}

	/**
	 * Gets the deferred period type id.
	 *
	 * @return the deferred period type id
	 */
	public Long getDeferredPeriodTypeId() {

		return deferredPeriodTypeId;
	}

	/**
	 * Sets the deferred period type id.
	 *
	 * @param deferredPeriodTypeId the new deferred period type id
	 */
	public void setDeferredPeriodTypeId(Long deferredPeriodTypeId) {

		this.deferredPeriodTypeId = deferredPeriodTypeId;
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

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.models.AcmModule;

/**
 * {@link AcmModule} class.
 *
 * @author kouali
 * @since 0.1.0
 */

public class AcmModuleDTO implements Serializable {

	/** The id module. */
	private Long idModule;

	/** The module. */

	private String module;

	/**
	 * Gets the id module.
	 *
	 * @return the idModule
	 */
	public Long getIdModule() {

		return idModule;
	}

	/**
	 * Sets the id module.
	 *
	 * @param idModule the idModule to set
	 */
	public void setIdModule(Long idModule) {

		this.idModule = idModule;
	}

	/**
	 * Gets the module.
	 *
	 * @return the module
	 */
	public String getModule() {

		return module;
	}

	/**
	 * Sets the module.
	 *
	 * @param module the module to set
	 */
	public void setModule(String module) {

		this.module = module;
	}

}

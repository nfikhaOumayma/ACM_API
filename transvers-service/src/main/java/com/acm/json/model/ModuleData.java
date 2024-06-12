/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link ModuleData} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class ModuleData {

	/** The module id. */
	@JsonProperty("ModuleId")
	public String moduleId;

	/** The content. */
	@JsonProperty("Content")
	public Content content;

	/**
	 * Instantiates a new module data.
	 */
	public ModuleData() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ModuleData [" + (moduleId != null ? "moduleId=" + moduleId + ", " : "")
				+ (content != null ? "content=" + content : "") + "]";
	}

}

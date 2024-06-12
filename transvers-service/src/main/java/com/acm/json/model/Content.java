/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.json.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link Content} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6.1
 */
public class Content {

	/** The d ATA. */
	@JsonProperty("DATA")
	public List<Datum> dATA;

	/**
	 * Instantiates a new content.
	 */
	public Content() {

		// EMPTY
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "Content [" + (dATA != null ? "dATA=" + dATA : "") + "]";
	}

}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import io.swagger.annotations.ApiModelProperty;

/**
 * {@link AcmStatutsDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
public class AcmStatutsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4909067249279382493L;

	/** The key. */
	@ApiModelProperty(name = "key", value = "Unique KEY", dataType = "Integer", required = true,
			example = "5")
	private Integer key;

	/** The value. */
	@ApiModelProperty(name = "value", value = "Unique VALUE", dataType = "String", required = true,
			example = "Add Documents")
	private String value;

	/**
	 * Instantiates a new acm statuts DTO.
	 */
	public AcmStatutsDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new acm statuts DTO.
	 *
	 * @param key the key
	 * @param value the value
	 */
	public AcmStatutsDTO(Integer key, String value) {

		this.key = key;
		this.value = value;
	}

	/**
	 * Gets the key.
	 *
	 * @return the key
	 */
	public Integer getKey() {

		return key;
	}

	/**
	 * Sets the key.
	 *
	 * @param key the key to set
	 */
	public void setKey(Integer key) {

		this.key = key;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(String value) {

		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "AcmStatutsDTO [key=" + key + ", value=" + value + "]";
	}

}

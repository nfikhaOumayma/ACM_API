/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm;

import javax.validation.constraints.NotNull;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.core.io.Resource;
import org.springframework.validation.annotation.Validated;

import lombok.Data;

/**
 * {@link ApplicationProperties} class.
 *
 * @author HaythemBenizid
 * @since 1.1.4
 */
@Data
@Validated
@ConfigurationProperties(prefix = "com.acm")
public class ApplicationProperties {

	/** The base path where SQL config fil will be stored after compilation. */
	@NotNull
	private Resource storageLocation;

	/**
	 * Gets the storage location.
	 *
	 * @return the storageLocation
	 */
	public Resource getStorageLocation() {

		return storageLocation;
	}

	/**
	 * Sets the storage location.
	 *
	 * @param storageLocation the storageLocation to set
	 */
	public void setStorageLocation(Resource storageLocation) {

		this.storageLocation = storageLocation;
	}

}

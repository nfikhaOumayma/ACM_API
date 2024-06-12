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
 * @since 0.1.0
 */
@Data
@Validated
@ConfigurationProperties(prefix = "com.acm")
public class ApplicationProperties {

	/** The base path where reports will be stored after compilation. */
	@NotNull
	private Resource storageLocation;

	/** The location of JasperReports source files. */
	@NotNull
	private Resource reportLocation;

	/** The base path where reports will be stored after compilation. */
	@NotNull
	private Resource fileStorageLocation;

	/** The storage location jrxml. */
	@NotNull
	private Resource storageLocationJrxml;

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

	/**
	 * Gets the report location.
	 *
	 * @return the reportLocation
	 */
	public Resource getReportLocation() {

		return reportLocation;
	}

	/**
	 * Sets the report location.
	 *
	 * @param reportLocation the reportLocation to set
	 */
	public void setReportLocation(Resource reportLocation) {

		this.reportLocation = reportLocation;
	}

	/**
	 * Gets fileStorageLocation.
	 *
	 * @return value of fileStorageLocation
	 */
	public Resource getFileStorageLocation() {

		return fileStorageLocation;
	}

	/**
	 * Sets the fileStorageLocation.
	 *
	 * @param fileStorageLocation the new value
	 */
	public void setFileStorageLocation(Resource fileStorageLocation) {

		this.fileStorageLocation = fileStorageLocation;
	}

	/**
	 * Gets the storage location jrxml.
	 *
	 * @return the storageLocationJrxml
	 */
	public Resource getStorageLocationJrxml() {

		return storageLocationJrxml;
	}

	/**
	 * Sets the storage location jrxml.
	 *
	 * @param storageLocationJrxml the storageLocationJrxml to set
	 */
	public void setStorageLocationJrxml(Resource storageLocationJrxml) {

		this.storageLocationJrxml = storageLocationJrxml;
	}
}

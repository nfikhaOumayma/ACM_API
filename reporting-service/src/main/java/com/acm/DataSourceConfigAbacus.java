/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm;

import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.core.env.Environment;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.zaxxer.hikari.HikariDataSource;

/**
 * The Class DataSourceConfigAbacus.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 * @link DataSourceConfigAbacus class.
 */
@Configuration
@EnableTransactionManagement
public class DataSourceConfigAbacus {

	/** The environment. */
	private final Environment environment; // Inject the Environment

	/**
	 * Instantiates a new data source config abacus.
	 *
	 * @param environment the environment
	 */
	public DataSourceConfigAbacus(Environment environment) {

		this.environment = environment;
	}

	/**
	 * Acm data source properties.
	 *
	 * @return the data source properties
	 */
	@Bean
	@Primary
	@ConfigurationProperties("spring.datasource")
	public DataSourceProperties acmDataSourceProperties() {

		return new DataSourceProperties();
	}

	/**
	 * Acm data source.
	 *
	 * @return the hikari data source
	 */
	@Bean
	@Primary
	public HikariDataSource acmDataSource() {

		return acmDataSourceProperties().initializeDataSourceBuilder().type(HikariDataSource.class)
				.build();
	}

	/**
	 * Abacus data source properties.
	 *
	 * @return the data source properties
	 */
	@Bean
	@ConfigurationProperties("spring.datasource-abacus")
	public DataSourceProperties abacusDataSourceProperties() {

		return new DataSourceProperties();
	}

	/**
	 * Abacus data source.
	 *
	 * @return the hikari data source
	 */
	@Bean
	public HikariDataSource abacusDataSource() {

		return abacusDataSourceProperties().initializeDataSourceBuilder()
				.type(HikariDataSource.class).build();
	}

}

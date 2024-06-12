/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.config.ged;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * {@link GedConfigProperties} class.
 *
 * @author kouali
 * @since 0.1.0
 */
@ConfigurationProperties(prefix = "ged")
@Component
public class GedConfigProperties {

	/** The url. */
	private String url;

	/** The login. */
	private String login;

	/** The password. */
	private String password;

	/**
	 * Gets the url.
	 *
	 * @return the url
	 */
	public String getUrl() {

		return url;
	}

	/**
	 * Sets the url.
	 *
	 * @param url the url to set
	 */
	public void setUrl(String url) {

		this.url = url;
	}

	/**
	 * Gets the ged config.
	 *
	 * @param config the config
	 * @return the ged config
	 */
	public String getGedConfig(String config) {

		return url + config;
	}

	/**
	 * Gets the login.
	 *
	 * @return the login
	 */
	public String getLogin() {

		return login;
	}

	/**
	 * Sets the login.
	 *
	 * @param login the login to set
	 */
	public void setLogin(String login) {

		this.login = login;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPassword() {

		return password;
	}

	/**
	 * Sets the password.
	 *
	 * @param password the password to set
	 */
	public void setPassword(String password) {

		this.password = password;
	}

}

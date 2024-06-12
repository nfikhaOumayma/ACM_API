/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * {@link UsersNotifications} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
@Entity
@Table(name = "ACM_USERS_NOTIFICATION")
public class UsersNotifications extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1001264269410939138L;

	/** The id users notification. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_USERS_NOTIFICATION", unique = true, nullable = false)
	private Long idUsersNotification;

	/** The statut. */
	@Column(name = "STATUT")
	private Boolean statut;

	/** The user. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "USERNAME")
	private User user;

	/** The setting notification. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_SETTING_NOTIFICATION")
	private SettingNotifications settingNotification;

	/**
	 * Instantiates a new notifications.
	 */
	public UsersNotifications() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id users notification.
	 *
	 * @return the idUsersNotification
	 */
	public Long getIdUsersNotification() {

		return idUsersNotification;
	}

	/**
	 * Sets the id users notification.
	 *
	 * @param idUsersNotification the idUsersNotification to set
	 */
	public void setIdUsersNotification(Long idUsersNotification) {

		this.idUsersNotification = idUsersNotification;
	}

	/**
	 * Gets the statut.
	 *
	 * @return the statut
	 */
	public Boolean getStatut() {

		return statut;
	}

	/**
	 * Sets the statut.
	 *
	 * @param statut the statut to set
	 */
	public void setStatut(Boolean statut) {

		this.statut = statut;
	}

	/**
	 * Gets the user.
	 *
	 * @return the user
	 */
	public User getUser() {

		return user;
	}

	/**
	 * Sets the user.
	 *
	 * @param user the user to set
	 */
	public void setUser(User user) {

		this.user = user;
	}

	/**
	 * Gets the setting notification.
	 *
	 * @return the settingNotification
	 */
	public SettingNotifications getSettingNotification() {

		return settingNotification;
	}

	/**
	 * Sets the setting notification.
	 *
	 * @param settingNotification the settingNotification to set
	 */
	public void setSettingNotification(SettingNotifications settingNotification) {

		this.settingNotification = settingNotification;
	}

}

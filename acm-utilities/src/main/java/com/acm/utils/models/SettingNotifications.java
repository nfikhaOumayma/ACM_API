/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link SettingNotifications} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
@Entity
@Table(name = "ACM_SETTING_NOTIFICATION")
public class SettingNotifications extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -514440356171368453L;

	/** The id setting notification. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_NOTIFICATION", unique = true, nullable = false)
	private Long idSettingNotification;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The type motif. */
	@Column(name = "TYPE_NOTIF")
	private String typeNotif;

	/** The users notifications. */
	@OneToMany(mappedBy = "settingNotification")
	private Set<UsersNotifications> usersNotifications = new HashSet<>();

	/** The type. */
	@Column(name = "TYPE")
	private String type;

	/**
	 * Instantiates a new notifications.
	 */
	public SettingNotifications() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting notifications.
	 *
	 * @param idSettingNotification the id setting notification
	 */
	public SettingNotifications(Long idSettingNotification) {

		this.idSettingNotification = idSettingNotification;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the category to set
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the type notif.
	 *
	 * @return the typeNotif
	 */
	public String getTypeNotif() {

		return typeNotif;
	}

	/**
	 * Sets the type notif.
	 *
	 * @param typeNotif the typeNotif to set
	 */
	public void setTypeNotif(String typeNotif) {

		this.typeNotif = typeNotif;
	}

	/**
	 * Gets the id setting notification.
	 *
	 * @return the idSettingNotification
	 */
	public Long getIdSettingNotification() {

		return idSettingNotification;
	}

	/**
	 * Sets the id setting notification.
	 *
	 * @param idSettingNotification the idSettingNotification to set
	 */
	public void setIdSettingNotification(Long idSettingNotification) {

		this.idSettingNotification = idSettingNotification;
	}

	/**
	 * Sets the users notifications.
	 * 
	 * @return the usersNotifications
	 */
	public Set<UsersNotifications> getUsersNotifications() {

		return usersNotifications;
	}

	/**
	 * Gets the users notifications.
	 * 
	 * @param usersNotifications the usersNotifications to set
	 */
	public void setUsersNotifications(Set<UsersNotifications> usersNotifications) {

		this.usersNotifications = usersNotifications;
	}

	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public String getType() {

		return type;
	}

	/**
	 * Sets the type.
	 *
	 * @param type the type to set
	 */
	public void setType(String type) {

		this.type = type;
	}

}

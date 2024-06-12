/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingNotificationsDTO} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
public class SettingNotificationsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6359856052816164333L;

	/** The id setting notification. */
	private Long idSettingNotification;

	/** The category. */
	private String category;

	/** The creaction date. */
	private String typeNotif;

	/** The enabled. */
	private Boolean enabled;

	/** The insertBy. */
	private String insertBy;

	/** The update user notification. */
	private Boolean updateUserNotification;

	/** The type. */
	private String type;

	/**
	 * Instantiates a new setting notifications.
	 */
	public SettingNotificationsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new setting notification DTO.
	 *
	 * @param idSettingNotification the id setting notification
	 * @param category the category
	 * @param typeNotif the type notif
	 * @param enabled the enabled
	 * @param insertBy the insert by
	 */
	public SettingNotificationsDTO(Long idSettingNotification, String category, String typeNotif,
			Boolean enabled, String insertBy) {

		this.idSettingNotification = idSettingNotification;
		this.category = category;
		this.typeNotif = typeNotif;
		this.enabled = enabled;
		this.insertBy = insertBy;
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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insertBy
	 */
	public String getInsertBy() {

		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the insertBy to set
	 */
	public void setInsertBy(String insertBy) {

		this.insertBy = insertBy;
	}

	/**
	 * Gets the update user notification.
	 *
	 * @return the update user notification
	 */
	public Boolean getUpdateUserNotification() {

		return updateUserNotification;
	}

	/**
	 * Sets the update user notification.
	 *
	 * @param updateUserNotification the new update user notification
	 */
	public void setUpdateUserNotification(Boolean updateUserNotification) {

		this.updateUserNotification = updateUserNotification;
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

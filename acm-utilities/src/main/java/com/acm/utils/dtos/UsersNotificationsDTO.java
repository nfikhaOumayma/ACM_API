/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link UsersNotificationsDTO} class.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
public class UsersNotificationsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1645541535474814956L;

	/** The id user notification. */
	private Long idUsersNotification;

	/** The statut. */
	private Boolean statut;

	/** The insert by. */
	private String insertBy;

	/** The enabled. */
	private Boolean enabled;

	/** The user DTO. */
	@Mapping("user")
	private UserDTO userDTO;

	/** The setting notification DTO. */
	@Mapping("settingNotification")
	private SettingNotificationsDTO settingNotificationDTO;

	/**
	 * Instantiates a new user notifications.
	 */
	public UsersNotificationsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new users notifications DTO.
	 *
	 * @param idUsersNotification the id user notification
	 * @param statut the statut
	 * @param insertBy the insert by
	 * @param userDTO the user DTO
	 * @param settingNotificationDTO the setting notification DTO
	 */
	public UsersNotificationsDTO(Long idUsersNotification, Boolean statut, String insertBy,
			UserDTO userDTO, SettingNotificationsDTO settingNotificationDTO) {

		this.idUsersNotification = idUsersNotification;
		this.statut = statut;
		this.insertBy = insertBy;
		this.userDTO = userDTO;
		this.settingNotificationDTO = settingNotificationDTO;
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
	 * Gets the user DTO.
	 *
	 * @return the userDTO
	 */
	public UserDTO getUserDTO() {

		return userDTO;
	}

	/**
	 * Sets the user DTO.
	 *
	 * @param userDTO the userDTO to set
	 */
	public void setUserDTO(UserDTO userDTO) {

		this.userDTO = userDTO;
	}

	/**
	 * Gets the setting notification DTO.
	 *
	 * @return the settingNotificationDTO
	 */
	public SettingNotificationsDTO getSettingNotificationDTO() {

		return settingNotificationDTO;
	}

	/**
	 * Sets the setting notification DTO.
	 *
	 * @param settingNotificationDTO the settingNotificationDTO to set
	 */
	public void setSettingNotificationDTO(SettingNotificationsDTO settingNotificationDTO) {

		this.settingNotificationDTO = settingNotificationDTO;
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
	 * @param enabled the new enabled
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

}

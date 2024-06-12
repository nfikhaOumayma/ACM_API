/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;

import com.acm.utils.models.User;
import com.acm.utils.models.UsersNotifications;
import com.acm.utils.models.SettingNotifications;

/**
 * The {@link UserNotificationRepository} Interface.
 *
 * @author MoezMhiri
 * @since 0.12.0
 */
public interface UserNotificationRepository extends JpaRepository<UsersNotifications, Long>,
		QuerydslPredicateExecutor<UsersNotifications>, CrudRepository<UsersNotifications, Long> {

	/**
	 * find user notification by user code (only Enabled DATA).
	 *
	 * @author MoezMhiri
	 * @param user the user
	 * @param statut the statut
	 * @return the list
	 */
	List<UsersNotifications> findByUserAndStatut(User user, Boolean statut);

	/**
	 * Find by enabled not and statut not and setting notification.
	 *
	 * @author HaythemBenizid
	 * @param enabled the enabled
	 * @param statut the statut
	 * @param settingNotification the setting notification
	 * @return the list
	 */
	List<UsersNotifications> findByEnabledNotAndStatutNotAndSettingNotification(Boolean enabled,
			Boolean statut, SettingNotifications settingNotification);

}

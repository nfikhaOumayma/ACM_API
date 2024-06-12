/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.SettingNotifications;

/**
 * Class provides service dao for {@link SettingNotifications} table.
 *
 * @author HaythemBenizid
 * @since 1.0.1
 */
@Repository
public interface SettingNotificationsRepository extends JpaRepository<SettingNotifications, Long>,
		QuerydslPredicateExecutor<SettingNotifications>, CrudRepository<SettingNotifications, Long>,
		PagingAndSortingRepository<SettingNotifications, Long> {

	/**
	 * Find by type.
	 *
	 * @author kouali
	 * @param globalSettingNotif the global setting notif
	 * @return the list
	 */
	List<SettingNotifications> findByType(String globalSettingNotif);

	/**
	 * Find by type and enabled.
	 *
	 * @author kouali
	 * @param type the type
	 * @param b the b
	 * @return the list
	 */
	List<SettingNotifications> findByTypeAndEnabled(String type, boolean b);

}

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

import com.acm.utils.models.Notifications;

/**
 * {@link NotificationsRepository} class.
 *
 * @author YesserSomai
 * @since 0.10.0
 */
public interface NotificationsRepository
		extends JpaRepository<Notifications, Long>, QuerydslPredicateExecutor<Notifications>,
		CrudRepository<Notifications, Long>, PagingAndSortingRepository<Notifications, Long> {

	/**
	 * Find by status notif and username Order By Creation Date.
	 *
	 * @author YesserSomai
	 * @author HaythemBenizid
	 * @param statusNotif the status notif
	 * @param username the username
	 * @return the list
	 */
	List<Notifications> findFirst10ByStatusNotifAndUsernameOrderByCreactionDateDesc(
			String statusNotif, String username);

	/**
	 * Count by status notif and username order by creaction date desc.
	 * 
	 * @author HaythemBenizid
	 * @param statusNotif the status notif
	 * @param username the username
	 * @return the long
	 */
	Long countByStatusNotifAndUsernameOrderByCreactionDateDesc(String statusNotif, String username);
}

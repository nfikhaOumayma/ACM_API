/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.acm.utils.enums.UserCategory;
import com.acm.utils.models.User;

/**
 * {@link UserRepository} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Repository
public interface UserRepository extends JpaRepository<User, String>, CrudRepository<User, String>,
		PagingAndSortingRepository<User, String>, QuerydslPredicateExecutor<User> {

	/**
	 * Find by username ignore case.
	 * 
	 * @author HaythemBenizid
	 * @param username the username
	 * @return the user
	 */
	User findByUsernameIgnoreCase(String username);

	/**
	 * Find all collaborators by given responsable id.
	 *
	 * @author HaythemBenizid
	 * @param responsableId the responsable id
	 * @return the list
	 */
	List<User> findByResponsableId(String responsableId);

	/**
	 * Find all collaborators by given branch id.
	 *
	 * @author MoezMhiri
	 * @param branchId the branch id
	 * @return the list
	 */
	List<User> findByBranchID(Integer branchId);

	/**
	 * Find All user by given by groupe ID.
	 * 
	 * @author HaythemBenizid
	 * @param idGroupe the id groupe
	 * @return the list
	 */
	@Query("SELECT u FROM User u JOIN u.groupes grp WHERE grp.id = :idGroupe")
	List<User> findByGroupe(@Param("idGroupe") Long idGroupe);

	/**
	 * Find by groupe code and branch ID.
	 *
	 * @author HaythemBenizid
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	@Query(value = "SELECT * FROM ACM_USERS WHERE USERNAME IN (SELECT USERNAME FROM ACM_USERS_GROUPE WHERE ID_ACM_GROUPE = (SELECT ID_ACM_GROUPE FROM ACM_GROUPE WHERE CODE = ?1)) and BranchID = ?2",
			nativeQuery = true)
	List<User> findByGroupeCodeAndBranchID(String codeGroupe, Integer branchId);

	/**
	 * Find by category {@link UserCategory}.
	 * 
	 * @author HaythemBenizid
	 * @param category the category
	 * @return the list
	 */
	List<User> findByCategory(String category);

	/**
	 * Find by category and access branches containing (LIKE '%branchId%') and enabled.
	 *
	 * @author HaythemBenizid
	 * @param category the category
	 * @param branchId the branch id
	 * @param enabled the enabled
	 * @return the list
	 */
	List<User> findByCategoryAndAccessBranchesContainingAndEnabled(String category, String branchId,
			Boolean enabled);

	/**
	 * Find by BRANCH_ID in given List and ENABLED and category NOT CUSTOMER.
	 *
	 * @author HaythemBenizid
	 * @param listBranchIds the list branch ids
	 * @param enabled the enabled
	 * @param category the category
	 * @return the list
	 */
	List<User> findByBranchIDInAndEnabledAndCategoryNot(List<Integer> listBranchIds,
			Boolean enabled, String category);

	/**
	 * Find by groupe code and branch id and access branches.
	 * 
	 * @author idridi
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @param branchIdRight the branch id right
	 * @param branchIdMiddle the branch id middle
	 * @param branchIdLeft the branch id left
	 * @return the list
	 */
	@Query(value = "SELECT * FROM ACM_USERS where USERNAME in (SELECT USERNAME FROM ACM_USERS_GROUPE WHERE ID_ACM_GROUPE = (SELECT ID_ACM_GROUPE FROM ACM_GROUPE WHERE CODE = ?1)) and (BranchID=?2 or ACCESS_BRANCHES='?2' or ACCESS_BRANCHES like ?3 or ACCESS_BRANCHES like ?4 or ACCESS_BRANCHES like ?5) and ACM_ENABLED=1",
			nativeQuery = true)
	List<User> findByGroupeCodeAndBranchIdAndAccessBranches(String codeGroupe, Integer branchId,
			String branchIdRight, String branchIdMiddle, String branchIdLeft);

	/**
	 * Count by enabled.
	 *
	 * @author kouali
	 * @param b the b
	 * @return the integer
	 */
	Integer countByEnabled(boolean b);

}

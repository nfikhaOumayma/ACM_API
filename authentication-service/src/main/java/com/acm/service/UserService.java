/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CustomException;
import com.acm.exceptions.type.OldPwdInvalidException;
import com.acm.exceptions.type.PwdConfirmInvalidException;
import com.acm.exceptions.type.ResetPwdException;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.UserPaginationDTO;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.enums.UserHierarchicalType;

/**
 * {@link UserService} interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface UserService {

	/**
	 * Find {@link List} of {@link UserDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	List<UserDTO> find(UserDTO userDTO);

	/**
	 * Find responsible.
	 * 
	 * @author idridi
	 * @param userDTO the user DTO
	 * @return the list
	 */
	List<UserDTO> findResponsible(UserDTO userDTO);

	/**
	 * Find by groupe code and branch ID (used in Report).
	 *
	 * @author HaythemBenizid
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	List<UserDTO> findByGroupeCodeAndBranchID(String codeGroupe, Integer branchId);

	/**
	 * Find by given Login.
	 * 
	 * @author HaythemBenizid
	 * @param login the login
	 * @return the user DTO
	 */
	UserDTO find(String login);

	/**
	 * The method used for saving the given {@link UserDTO}.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws CustomException the custom exception
	 */
	UserDTO save(UserDTO userDTO) throws CustomException;

	/**
	 * The method used to save an IB user.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	UserDTO saveForIB(UserDTO userDTO);

	/**
	 * Save for batch (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	UserDTO saveForBatch(UserDTO userDTO);

	/**
	 * Find list user (ALL {@link UserHierarchicalType}) for connected user && if FullList=True add
	 * list of user with {@link UserCategory}=MANAGMENT for his branch.
	 *
	 * @author MoezMhiri
	 * @author HaythemBenizid
	 * @param fullList the full list
	 * @return the list
	 */
	List<UserDTO> findUsers(Boolean fullList);

	/**
	 * Find {@link List} of {@link UserDTO}.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	List<UserDTO> find();

	/**
	 * The method used for updating pwd fot the given {@link UserDTO}.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws PwdConfirmInvalidException the pwd confirm invalid exception
	 * @throws OldPwdInvalidException the old pwd invalid exception
	 */
	UserDTO updatePWD(UserDTO userDTO) throws PwdConfirmInvalidException, OldPwdInvalidException;

	/**
	 * Update Only Enable of given User.
	 *
	 * @author YesserSomai
	 * @param userDTO the user DTO
	 * @return the list
	 */
	UserDTO saveEnable(UserDTO userDTO);

	/**
	 * The method used for udpate the given {@link UserDTO} by login.
	 *
	 * @author YesserSomai
	 * @param login the login
	 * @param userDTO the user DTO
	 * @return the user DTO
	 * @throws CustomException the custom exception
	 */
	UserDTO save(String login, UserDTO userDTO) throws CustomException;

	/**
	 * load users by branch id.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	List<UserDTO> findUsersByBranchId();

	/**
	 * Find portfolio.
	 * 
	 * @author AbdelkarimTurki
	 * @param userDTO the user DTO
	 * @return the list
	 */
	List<UserDTO> findPortfolio(UserDTO userDTO);

	/**
	 * Find All user by given by groupe Code.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	List<UserDTO> findByGroupe(UserDTO userDTO);

	/**
	 * Reset PWD.
	 *
	 * @author MoezMhiri
	 * @param login the login
	 * @return the user DTO
	 * @throws ResetPwdException the reset pwd exception
	 */
	UserDTO resetPWD(String login) throws ResetPwdException;

	/**
	 * Find {@link UserPaginationDTO} by page size & page number & given params
	 * ({@link UserPaginationDTO}).
	 *
	 * @author SalmeN Fatnassi
	 * @param userPaginationDTO the user pagination DTO
	 * @return the user pagination DTO
	 */
	UserPaginationDTO find(UserPaginationDTO userPaginationDTO);

	/**
	 * Update Only DefaultLang of given User.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	UserDTO saveDefaultLang(UserDTO userDTO);

	/**
	 * Find by groupe code and branch ID and access branches.
	 *
	 * @author idridi
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	List<UserDTO> findByGroupeCodeAndBranchIDAndAccessBranches(String codeGroupe, Integer branchId);

	/**
	 * Gets the users with loan branch in their access branches.
	 *
	 * @author ManelLamloum
	 * @param usersDTO the users DTO
	 * @param loanBranchId the loan branch id
	 * @return the users with loan branch in their access branches
	 */
	List<UserDTO> getUsersWithLoanBranchInTheirAccessBranches(List<UserDTO> usersDTO,
			Integer loanBranchId);

	/**
	 * Gets the responsable of user in parameter.
	 *
	 * @param userDTO the user DTO
	 * @return the responsable of user
	 */
	UserDTO findResponsibleOfUser(UserDTO userDTO);

	/**
	 * Update users responsible.
	 * 
	 * @author idridi
	 * @param usersDTOs the users DT os
	 * @return the list
	 */
	List<UserDTO> updateUsersResponsible(List<UserDTO> usersDTOs);

	/**
	 * Gets the pwd expire delay.
	 *
	 * @return the pwd expire delay
	 */
	Integer getPwdExpireDelay();

	/**
	 * Count user.
	 *
	 * @author kouali
	 * @return the integer
	 */
	Integer countUser();
}

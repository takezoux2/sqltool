CREATE TABLE `itemdata` (
  `itemId` bigint(20) NOT NULL,
  `itemType` int(11) NOT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
  `thumbnail` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
  `explanation` longtext CHARACTER SET utf8mb4 NOT NULL,
  `created` datetime NOT NULL,
  `updated` datetime NOT NULL,
  PRIMARY KEY (`itemId`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
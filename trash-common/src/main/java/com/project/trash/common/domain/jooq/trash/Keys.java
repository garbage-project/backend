/*
 * This file is generated by jOOQ.
 */
package trash;


import org.jooq.TableField;
import org.jooq.UniqueKey;
import org.jooq.impl.DSL;
import org.jooq.impl.Internal;

import trash.tables.Member;
import trash.tables.records.MemberRecord;


/**
 * A class modelling foreign key relationships and constraints of tables in
 * trash.
 */
@SuppressWarnings({ "all", "unchecked", "rawtypes", "this-escape" })
public class Keys {

    // -------------------------------------------------------------------------
    // UNIQUE and PRIMARY KEY definitions
    // -------------------------------------------------------------------------

    public static final UniqueKey<MemberRecord> KEY_MEMBER_PRIMARY = Internal.createUniqueKey(Member.MEMBER, DSL.name("KEY_MEMBER_PRIMARY"), new TableField[] { Member.MEMBER.MBR_SEQ }, true);
}

<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight()"
      :cell-class-name="handelCellClassName"
      style="width: 100%;"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="公司名称" min-width="160px" />
      <el-table-column v-if="columns.visible('socialCode')" key="socialCode" prop="socialCode" :show-overflow-tooltip="true" label="社会统一信用代码" min-width="160px" />
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" min-width="70px" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="200px" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" fixed="right" prop="createTime" label="创建时间" min-width="160px">
        <template v-slot="scope">
          <div>{{ scope.row.createTime | parseTime }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" key="enabled" prop="enabled" label="状态" align="center" min-width="70px">
        <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`公司被禁用后，该公司无法再在其他页面中显示`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.enabled"
            :disabled="!checkPermission(permission.edit)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeEnabled(scope.row, scope.row.enabled)"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit,...permission.get])"
        label="操作"
        width="230px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <el-popover
            placement="right"
            width="600"
            trigger="click"
          >
            <el-table :data="scope.row.bankAccounts">
              <el-table-column min-width="300" property="depositBank" label="开户行" />
              <el-table-column min-width="300" property="account" label="账号" />
            </el-table>
            <el-button slot="reference" type="success" size="mini">查看银行账号</el-button>
          </el-popover>
          <udOperation
            :data="scope.row"
          />
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script>
import crudMethod, { editStatus } from '@/api/config-manage/base/branch-company'
import CRUD, { presenter } from '@crud/crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import checkPermission from '@/utils/permission'
import enumOperate, { enabledEnum } from '@/utils/enum/index'

const enabledEnumV = enumOperate.getVal(enabledEnum)

// crud交由presenter持有
const permission = {
  get: ['branchCompany:get'],
  add: ['branchCompany:add'],
  edit: ['branchCompany:edit'],
  del: ['branchCompany:del']
}

const crud = CRUD({
  title: '分支机构',
  sort: ['sort.asc', 'id.desc'],
  permission: { ...permission },
  crudMethod: { ...crudMethod }
})

export default {
  name: 'ConfigBranchCompany',
  components: { mHeader, mForm, pagination, udOperation },
  mixins: [presenter(crud)],
  inject: ['$_tableMaxHeight'],
  data() {
    return {
      permission,
      enabledEnum,
      enabledEnumV
    }
  },
  methods: {
    checkPermission,
    async changeEnabled(data, val) {
      try {
        await this.$confirm('此操作将 "' + enabledEnumV[val].L + '" ' + data.name + ', 是否继续？', '提示', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        })
        await editStatus({ id: data.id, enabled: val })
        this.crud.refresh()
        this.crud.notify(enabledEnumV[val].L + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
      } catch (error) {
        console.log('变更公司状态', error)
        data.enabled = data.enabled === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
      }
    },
    handelCellClassName({ row, column, rowIndex, columnIndex }) {
      const markColumn = ['name'] // 标记字段
      let className = ''
      if (markColumn.includes(column.property)) {
        if (column.property === 'name' && row['isParent'] === true) {
          className = 'parent-mark'
        }
        className += ' marked'
      }
      return className
    }
  }
}
</script>
<style lang="scss" scoped>
$default-cell-mask-color: #ff000021 !default;
/deep/.parent-mark {
  overflow: hidden!important;
  .cell {
      &:after {
          content: '母公司';
          background: #e64242;
          transform:  rotate(-45deg);
          color: white;
          font-weight: 100;
          position: absolute;
          top: 5px;
          left: -20px;
          right: 0;
          width: 70px;
          height: 20px;
          font-size: 11px;
          display: flex;
          justify-content: center;
          align-items: center;
          pointer-events: none; // 穿透
      }
  }
}
/deep/.marked {
  .cell {
    padding-left: 24px;
  }
}
</style>

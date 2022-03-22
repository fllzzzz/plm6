<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="公司名称" min-width="150">
        <template v-slot="scope">
          <table-cell-tag :show="scope.row.isParent === systemEnabledEnum.ENUM.TRUE.V" name="母公司" />
          <div>{{ scope.row.name }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('socialCode')"
        key="socialCode"
        prop="socialCode"
        :show-overflow-tooltip="true"
        label="社会统一信用代码"
        min-width="150"
      >
        <template v-slot="scope">
          <div>{{ scope.row.socialCode }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" :show-overflow-tooltip="true" label="排序" min-width="80">
        <template v-slot="scope">
          <span>{{ scope.row.sort }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" label="备注" align="center" min-width="260">
        <template v-slot="scope">
          <span>{{ scope.row.remark }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" label="状态" align="center" min-width="260">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.enabled"
            :disabled="!checkPermission(permission.edit)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="systemEnabledEnum.ENUM.TRUE.V"
            :inactive-value="systemEnabledEnum.ENUM.FALSE.V"
            @change="changeEnabled(scope.row, scope.row.enabled)"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <ud-operation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/config/system-config/branch-company'
import { ref } from 'vue'
import { branchCompanyPM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import { systemEnabledEnum } from '@enum-ms/system'
import { ElMessageBox } from 'element-plus'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '分支机构',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.branchCompanyConfig',
  paginate: true,
  extraHeight: 40
})

async function changeEnabled(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + systemEnabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, enabled: val })
    crud.refresh()
    crud.notify(systemEnabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更公司状态', error)
    data.enabled = data.enabled === systemEnabledEnum.ENUM.TRUE.V ? systemEnabledEnum.ENUM.FALSE.V : systemEnabledEnum.ENUM.TRUE.V
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    v.bankAccounts = v.bankAccounts || []
    return v
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>

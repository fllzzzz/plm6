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
      @row-dblclick="changeDefault"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('templateName')"
        key="templateName"
        prop="templateName"
        :show-overflow-tooltip="true"
        label="模板名称"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <table-cell-tag :show="scope.row.isDefault" name="默认" />
          <div>{{ scope.row.templateName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="描述信息"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.remark }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" label="状态" align="center" min-width="260">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.status"
            :disabled="!checkPermission(permission.edit)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="systemEnabledEnum.ENUM.TRUE.V"
            :inactive-value="systemEnabledEnum.ENUM.FALSE.V"
            @change="changeStatus(scope.row, scope.row.status)"
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
          <udOperation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editDefault, editStatus } from '@/api/contract/member-config'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { systemEnabledEnum } from '@enum-ms/system'
import { ElMessageBox } from 'element-plus'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

// crud交由presenter持有
const permission = {
  get: ['memberConfig:get'],
  add: ['memberConfig:add'],
  edit: ['memberConfig:edit'],
  del: ['memberConfig:del'],
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false,
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '项目成员模板',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.memberConfig',
  paginate: true,
  extraHeight: 157,
})

async function changeDefault(row) {
  try {
    const isDefault = !row.isDefault
    const tip = isDefault ? `此操作将“${row.templateName}”设置为该表格的默认模板` : `此操作取消“${row.templateName}”的默认状态`
    await ElMessageBox.confirm(tip, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning',
    })
    await editDefault({ id: row.id, isDefault })
    crud.refresh()
    crud.notify(isDefault ? '设置' : '取消' + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更表格状态', error)
  }
}

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + systemEnabledEnum.VL[val] + '" ' + data.templateName + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning',
    })
    await editStatus({ id: data.id, status: val })
    crud.refresh()
    crud.notify(systemEnabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更成员模板状态', error)
    data.status = data.status === systemEnabledEnum.ENUM.TRUE.V ? systemEnabledEnum.ENUM.FALSE.V : systemEnabledEnum.ENUM.TRUE.V
  }
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
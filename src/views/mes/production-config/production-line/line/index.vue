<template>
  <el-card class="line-box box-card">
    <template v-slot:header>
      <span style="line-height: 28px">{{ crud.title }}列表</span>
      <!-- 新增 -->
      <common-button
        v-permission="permission.add"
        style="float: right; padding: 6px 10px"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click="crud.toAdd"
      >
        新增
      </common-button>
    </template>
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      highlight-current-row
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @current-change="handleCurrentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="生产线名称"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="生产线简称"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('boolEnabledEnum')"
        key="boolEnabledEnum"
        prop="boolEnabledEnum"
        label="状态"
        align="center"
        width="80px"
      >
        <template v-slot:header>
          <el-tooltip class="item" effect="light" :content="`生产线被禁用后，该生产线无法再在其他页面中显示`" placement="top">
            <div style="display: inline-block">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.boolEnabledEnum"
            :disabled="!useCheckPermission(permission.editStatus)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeStatus(scope.row, scope.row.boolEnabledEnum)"
          />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="80px" />
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="140px"
      />
      <!--编辑与删除-->
      <el-table-column
        v-if="useCheckPermission([...permission.del, ...permission.edit])"
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
  </el-card>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/mes/production-config/production-line'
import { ref, defineEmits } from 'vue'
import { ElMessageBox } from 'element-plus'

import { enabledEnum } from '@enum-ms/common'
import useCheckPermission from '@compos/use-check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const emit = defineEmits(['click-line'])

// crud交由presenter持有
const permission = {
  get: ['productionLine:get'],
  add: ['productionLine:add'],
  edit: ['productionLine:edit'],
  del: ['productionLine:del'],
  editStatus: ['productionLine:editStatus']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产线',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['shortName']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.line-box',
  paginate: true,
  extraHeight: 157
})

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus({ id: data.id, boolEnabledEnum: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更生产线状态', error)
    data.boolEnabledEnum = data.boolEnabledEnum === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

function handleCurrentChange(val) {
  if (val) {
    emit('click-line', val)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.line-box) {
  .el-card__body {
    padding-top: 11px;
    .el-tabs {
      margin-bottom: 7px;
    }
  }
  .card-header {
    height: 28px;
  }
}
</style>

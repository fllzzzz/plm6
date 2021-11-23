<template>
  <el-card class="line-box box-card">
    <template v-slot:header>
      <span style="line-height: 28px">{{ crud.title }}列表</span>
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
      style="width: 100%;margin-top:10px;"
      @current-change="handleCurrentChange"
      v-if="crud.query.type!=TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="描述"
        min-width="140px"
      />
    </common-table>
    <common-table
      ref="multipleTable"
      v-loading="crud.loading"
      highlight-current-row
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      @current-change="handleCurrentChange"
      v-else
    >
    <!-- <common-table
      ref="tableRef"
      v-loading="crud.loading"
      highlight-current-row
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      @current-change="handleCurrentChange"
      @selection-change="handleSelectionChange"
      v-else
    > -->
      <el-table-column key="selection" type="selection" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('code')"
        key="code"
        prop="code"
        :show-overflow-tooltip="true"
        label="编码"
      />
      <el-table-column key="status" prop="status" label="状态">
          <template v-slot="scope">
            <el-switch
            v-model="scope.row.status"
            :disabled="!checkPermission(permission.editStatus)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeStatus(scope.row, scope.row.status)"
          />
          </template>
        </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </el-card>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/contract/enclosure-config/enclosure'
import { ref, defineEmits } from 'vue'
import { TechnologyTypeEnum } from '@enum-ms/contract'
import { ElMessageBox } from 'element-plus'

import { enabledEnum } from '@enum-ms/common'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const emit = defineEmits(['click-line'])

// crud交由presenter持有
const permission = {
  get: ['enclosureConfig:get'],
  editStatus: ['enclosureConfig:editStatus']
}

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const multipleTable = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef,
  multipleTable
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.dict-box',
  paginate: true,
  extraHeight: 157
})

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + enabledEnum.VL[val] + '" ' + data.code + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const submitData = {
      type: this.crud.query.type,
      data: { code: data.code, updateStatus: 1 }
    }
    await editStatus(submitData)
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更桁架楼层板配置状态', error)
    data.status = data.status === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

function handleCurrentChange(val) {
  if (val) {
    emit('click-line', val)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  if (data.data.content.length > 0) {
    data.data.content.forEach(v => {
      v.type = crud.query.type
    })
  }
  return data
}
</script>

<style lang="scss" scoped>
::deep(.line-box) {
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
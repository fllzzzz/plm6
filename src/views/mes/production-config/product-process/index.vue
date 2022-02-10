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
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="名称" width="140px" />
      <el-table-column
        v-if="columns.visible('boolEnabledEnum')"
        key="boolEnabledEnum"
        prop="boolEnabledEnum"
        label="状态"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.boolEnabledEnum"
            :disabled="!checkPermission(permission.editStatus)"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="enabledEnum.TRUE.V"
            :inactive-value="enabledEnum.FALSE.V"
            @change="changeEnabled(scope.row, scope.row.boolEnabledEnum)"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('processType') && crud.query.sequenceType & typeEnum.ARTIFACT.V"
        key="processType"
        prop="processType"
        label="工序次序"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ processTypeEnum.VL[scope.row.processType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="工序"
        min-width="160px"
      />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="110px" />
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
import crudApi, { editStatus } from '@/api/mes/production-config/product-process'
import { ref } from 'vue'
import { ElMessageBox } from 'element-plus'

import { enabledEnum } from '@enum-ms/common'
import { processTypeEnum, processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'
import { configProductProcessPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工序管理',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

async function changeEnabled(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将"${enabledEnum.VL[val]}"${data.name}\n${val === enabledEnum.FALSE.V ? '并且会暂停所有使用此工序的生产线\n' : ''} 是否继续？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    await editStatus({ id: data.id, boolEnabledEnum: val })
    crud.refresh()
    crud.notify(enabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('操作构件工序状态', error)
    data.boolEnabledEnum = data.boolEnabledEnum === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.createTime = parseTime(v.createTime)
    v.process = JSON.parse(JSON.stringify(v.medBuildingProductProcessLinkList))
    if (v.process && v.process.length > 0) {
      v.processSequence = v.process.map((v) => `【${v.processName}】`).join('→')
      v.processSequenceIds = v.process.map((v) => v.processId)
    } else {
      v.processSequence = ''
      v.processSequenceIds = []
    }
    return v
  })
}

CRUD.HOOK.beforeToAdd = (crud, data) => {
  crud.form.processType = crud.query.processType
  crud.form.sequenceType = crud.query.sequenceType
}
</script>

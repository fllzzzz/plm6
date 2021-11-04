<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
      <!--工具栏-->
      <div class="head-container">
        <mHeader ref="header" :permission="permission" :line-id="lineId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('processName')"
          key="processName"
          prop="processName"
          :show-overflow-tooltip="true"
          label="工序名称"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('inspectorNames')"
          key="inspectorNames"
          prop="inspectorNames"
          :show-overflow-tooltip="true"
          label="质检"
          min-width="160px"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :permission="permission" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/production-line-inspection'
import { defineExpose, ref, defineProps, watch, computed } from 'vue'

import checkPermission from '@/utils/permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['productionLineInspect:get'],
  add: ['productionLineInspect:add'],
  edit: ['productionLineInspect:edit'],
  del: ['productionLineInspect:del']
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '质检',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const lineId = computed(() => {
  return props.line && props.line.id
})

watch(
  () => lineId,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.productionLineId = lineId
  return !!crud.query.productionLineId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.inspectors = JSON.parse(JSON.stringify(v.mesBuildingInspectionTeamUserLinkDTOList))
    if (v.inspectors && v.inspectors.length > 0) {
      v.inspectorNames = v.inspectors.map((v) => v.userName).join(', ')
      v.inspectorIds = v.inspectors.map((v) => v.userId)
    } else {
      v.inspectorNames = ''
      v.inspectorIds = []
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.productionLineId = lineId
  crud.form.factoryId = props.line.factoryId
  crud.form.workshopId = props.line.workshopId

  const members = crud.form.inspectors
  const userList = []
  for (let i = 0; i < members.length; i++) {
    if (members[i]) {
      userList.push({
        id: members[i].id,
        userId: members[i].id,
        userName: members[i].name,
        inspectionTeamId: this.crud.form.id
      })
    }
  }
  crud.form.mesBuildingInspectionTeamUserLinkList = userList
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>

<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <artifact-project-list :maxHeight="maxHeight - 40" @nesting-task-click="handleNestingTaskClick" />
    </div>
    <div class="wrap-right">
      <el-tag v-if="!crud.query?.areaIds?.length" type="info" size="medium"> * 请点击左侧项目列表查看详情 </el-tag>
      <template v-else>
        <div class="wrap-head">
          <mHeader />
        </div>
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :dataFormat="dataFormat"
          :max-height="maxHeight - 130"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="70">
            <template #default="{ row, $index }">
              <table-cell-tag :show="row.boolPrinted" color="#e64242" name="已打印" type="printed" />
              <span>{{ $index + 1 }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('scheduleTime')"
            :show-overflow-tooltip="true"
            prop="scheduleTime"
            label="排产日期"
            width="110px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('scheduleOrder')"
            :show-overflow-tooltip="true"
            prop="scheduleOrder"
            label="任务工单号"
            min-width="110px"
            align="center"
          >
            <template #default="{ row }">
              <table-cell-tag :show="row.boolIntellect" name="智" />
              <span>{{ row.scheduleOrder }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('userName')"
            :show-overflow-tooltip="true"
            prop="userName"
            label="排产人"
            min-width="110px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('productType')"
            :show-overflow-tooltip="true"
            prop="productType"
            label="类型"
            width="100px"
            align="center"
          >
            <template #default="{ row }">
              <el-tag :type="row.productType & componentTypeEnum.ASSEMBLE.V ? '' : 'success'" effect="plain">
                {{ componentTypeEnum.VL[row.productType] }}
              </el-tag>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('workshop.name')"
            :show-overflow-tooltip="true"
            prop="workshop.name"
            label="车间"
            min-width="100px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('productionLine.name')"
            :show-overflow-tooltip="true"
            prop="productionLine.name"
            label="生产线"
            min-width="100px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('group.name')"
            :show-overflow-tooltip="true"
            prop="group.name"
            label="生产组"
            min-width="80px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('taskQuantity')"
            :show-overflow-tooltip="true"
            prop="taskQuantity"
            label="任务数（件）"
            min-width="80px"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('taskNetWeight')"
            :show-overflow-tooltip="true"
            prop="taskNetWeight"
            label="总净重（kg）"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('taskGrossWeight')"
            :show-overflow-tooltip="true"
            prop="taskGrossWeight"
            label="总毛重（kg）"
            align="center"
          />
          <el-table-column v-permission="[...permission.detail]" label="操作" width="80px" align="center">
            <template #default="{ row }">
              <common-button v-permission="permission.detail" type="primary" size="mini" @click="showDetail(row)">查看</common-button>
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <pagination />
      </template>
      <!-- 查看 -->
      <detail v-model:visible="drawerVisible" :detail-data="detailData" @refresh="crud.toQuery" />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/work-order-manage/artifact.js'
import { ref, provide } from 'vue'

import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import { artifactWorkOrderPM as permission } from '@/page-permission/mes'
// import { debounce } from '@/utils'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from './module/detail.vue'
import artifactProjectList from './module/artifact-project-list.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '结构工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['taskGrossWeight'],
    requiredQuery: ['productType', 'areaIds']
  },
  tableRef
)

const dataFormat = ref([['scheduleTime', ['parse-time', '{y}-{m}-{d}']]])

provide('crud', crud)
provide('permission', permission)

const { maxHeight } = useMaxHeight()

// const handleNestingTaskClick = debounce(function (nodes = []) {
//   if (nodes?.length) {
//     console.log(nodes, 'nodes')
//     crud.query.areaId = nodes[0].id
//     crud.query.projectId = nodes[0].projectId
//   } else {
//     crud.query.areaId = undefined
//     crud.query.projectId = undefined
//   }
//   crud.toQuery()
// }, 500)

function handleNestingTaskClick({ areaIds, projectIds, monomerIds }) {
  // console.log(areaIds, 'areaIds')
  crud.query.areaIds = areaIds
  crud.query.projectIds = projectIds
  crud.query.monomerIds = monomerIds
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.boolPrinted = Boolean(v.printQuantity)
    v.boolIntellect = v.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V
    return v
  })
}

const itemInfo = ref()
const detailData = ref({})
const drawerVisible = ref(false)

function showDetail(row) {
  drawerVisible.value = true
  detailData.value = row.sourceRow
  console.log(row, detailData.value.taskQuantity, 'row')
  itemInfo.value = Object.assign({}, row)
}

</script>
<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 400px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
</style>

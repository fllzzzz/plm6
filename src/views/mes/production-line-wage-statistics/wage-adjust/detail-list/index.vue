<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <tag-tabs
        v-if="processList?.length"
        v-model="crud.query.processId"
        class="filter-item"
        style="'width:100%"
        :data="processList"
        itemKey="id"
        @change="crud.toQuery"
      >
        <template #default="{ item }">
          <span>{{ item.name }}</span>
        </template>
      </tag-tabs>
      <mHeader ref="headRef" :fInfo="fInfo">
        <template #btn>
          <common-button type="primary" size="mini" @click="batchHandle" :disabled="!selections?.length">批量调整</common-button>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="productFormat[taskTypeEnum]"
      :empty-text="crud.emptyText"
      :max-height="maxHeight - 50"
      row-key="rowId"
      style="width: 100%"
      @selection-change="handleSelectionChange"
    >
      <el-table-column type="selection" align="center" width="60" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('monomer.name')"
        show-overflow-tooltip
        prop="monomer.name"
        label="单体"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        show-overflow-tooltip
        prop="area.name"
        label="区域"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        show-overflow-tooltip
        prop="serialNumber"
        label="编号"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        show-overflow-tooltip
        prop="specification"
        label="规格"
        align="center"
        min-width="110px"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        show-overflow-tooltip
        prop="quantity"
        label="数量"
        align="center"
        min-width="70px"
      />
      <el-table-column
        v-if="columns.visible('netWeight')"
        show-overflow-tooltip
        prop="netWeight"
        label="重量(kg)"
        align="center"
        min-width="90px"
      />
      <!-- <el-table-column v-permission="permission.edit" align="center" prop="prop" label="操作" width="110">
        <template #default="{ row }">
          <common-button type="warning" size="mini" @click="handleSingleEdit(row)">工价调整</common-button>
        </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />
    <edit-dialog v-model:visible="editVisible" :selections="selections" :processInfo="processObj?.[crud.query.processId]" @refresh="crud.toQuery"></edit-dialog>
  </div>
</template>

<script setup>
import { detailGet, processGet } from '@/api/mes/production-line-wage-statistics/wage-adjust.js'
import { ref, defineProps, defineExpose, inject, watch, computed } from 'vue'
import { ElMessage } from 'element-plus'

import { componentTypeEnum } from '@enum-ms/mes'
import { arr2obj } from '@/utils/convert/type'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { productFormat } from '@/utils/columns-format/mes'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import editDialog from '@/views/mes/production-line-wage-statistics/wage-adjust/module/edit-dialog'
import tagTabs from '@comp-common/tag-tabs'

const permission = inject('permission')
const props = defineProps({
  fQuery: {
    type: Object,
    default: () => {}
  },
  fInfo: {
    type: Object,
    default: () => {}
  }
})

const taskTypeEnum = computed(() => {
  return props.fQuery?.taskTypeEnum || componentTypeEnum.ARTIFACT.V
})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const headRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工价调整',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: detailGet },
    sort: [],
    hasPagination: true,
    queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: true })

const processList = ref([])
const processObj = ref({})
const selections = ref([])
const editVisible = ref(false)

CRUD.HOOK.beforeToQuery = async (crud) => {
  crud.query.monomerId = props.fQuery?.monomerId
  crud.query.areaId = props.fQuery?.areaId
  crud.query.projectId = props.fQuery?.projectId
  crud.query.taskTypeEnum = props.fQuery?.taskTypeEnum
  crud.query.configId = props.fInfo?.productClass?.id
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.taskTypeEnum = props.fQuery?.taskTypeEnum
    v.processId = crud.query.processId
    v.sourceWage = v.wage
    v.sourceIntermediatePaintWage = v.intermediatePaintWage
    v.sourcePrimerWage = v.primerWage
    v.sourceTopcoatWage = v.topcoatWage
    return v
  })
}

watch(
  () => taskTypeEnum.value,
  () => {
    crud.data = []
    processList.value = []
    processObj.value = {}
  }
)

function handleSelectionChange(val) {
  selections.value = val
}

function batchHandle() {
  if (!selections.value?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }
  editVisible.value = true
}

async function fetchProcess(info) {
  try {
    const { content } = await processGet({ taskTypeEnum: taskTypeEnum.value, configId: info?.productClass?.id })
    processList.value = content
    processObj.value = arr2obj(content, 'id')
    if (processList.value?.length) {
      crud.query.processId = processList.value[0].id
      crud.toQuery()
    }
  } catch (error) {
    console.log(error, '获取工序失败')
  }
}

defineExpose({
  fetchProcess
})
</script>

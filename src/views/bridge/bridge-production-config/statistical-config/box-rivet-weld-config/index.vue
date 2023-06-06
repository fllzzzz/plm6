<template>
  <div class="app-container">
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
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column
        prop="classificationName"
        :show-overflow-tooltip="true"
        label="分段类型"
        align="center"
        min-width="200px"
        fixed="left"
      />
      <el-table-column prop="specPrefixStr" :show-overflow-tooltip="true" label="截面前缀" align="center" min-width="200px" fixed="left" />
      <el-table-column :show-overflow-tooltip="true" label="数值" align="center" min-width="200px" fixed="left">
        <template #default="{ row }">
          <span>{{ row.minNumerical }}</span>
          <span> ~ </span>
          <span>{{ row.maxNumerical }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="工序单价（元）" align="center" min-width="200px">
        <el-table-column
          v-for="item in processList"
          :key="item.id"
          :show-overflow-tooltip="true"
          align="center"
          :label="item.name"
          width="120px"
        >
          <template #default="{ row: { sourceRow: row } }">
            <span v-if="row.needProcessIds?.indexOf(item.id) !== -1">
              <span v-if="isNotBlank(row.processObj[item.id]?.price)">
                {{ row.processObj[item.id]?.price }}
                {{ wageQuotaTypeEnum.V[row.processObj[item.id]?.wageQuotaType].unit }}
              </span>
              <span v-else>-</span>
            </span>
            <!-- <span v-else>\</span> -->
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column min-width="1px" />
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="130px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <udOperation :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
    <m-batch-form :currentType="crud.query.id" />
  </div>
</template>

<script setup>
import crudApi, { getRivetWeld, getProcess, getBoxProcess } from '@/api/bridge/production-config/artifact-rivet-weld-config'
import { onMounted, provide, ref } from 'vue'

import { configBoxRivetWeldConfigPM as permission } from '@/page-permission/config'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { isNotBlank } from '@data-type/index'
import { arr2obj } from '@/utils/convert/type'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mBatchForm from './module/batch-form'
import mForm from './module/form'

const optShow = {
  batchAdd: true,
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '分段-组铆焊价格配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    requiredQuery: ['id']
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

const rivetWeldList = ref([])
const rivetWeldListObj = ref({})
const artifactTypeList = ref([])
const artifactTypeListObj = ref({})
const processList = ref([])
const processListObj = ref({})

provide('rivetWeldList', rivetWeldList)
provide('rivetWeldListObj', rivetWeldListObj)
provide('artifactTypeList', artifactTypeList)
provide('artifactTypeListObj', artifactTypeListObj)
provide('processList', processList)
provide('processListObj', processListObj)

async function fetchPreloadData() {
  try {
    const content = await getBoxProcess()
    artifactTypeList.value = content.map((v) => {
      v.specPrefixStr = v.specPrefixList.map((o) => o.specPrefix).join(' / ')
      const _obj = {}
      const _processIds = []
      if (v.productProcessLinkList) {
        for (const item of v.productProcessLinkList) {
          _obj[item['processId']] = {
            processLinkId: item.id,
            processId: item.processId
          }
          _processIds.push(item.processId)
        }
      }
      v.typeProcessObj = _obj
      v.processIds = _processIds
      return v
    })
    console.log(artifactTypeList.value)
    artifactTypeListObj.value = arr2obj(artifactTypeList.value, 'id')
    console.log(artifactTypeListObj.value)
  } catch (error) {
    console.log(error, '获取分段类型失败')
  }
  try {
    const { content } = await getRivetWeld()
    rivetWeldList.value = content
    rivetWeldListObj.value = arr2obj(content, 'id')
  } catch (error) {
    console.log(error, '获取分段种类配置失败')
  }
  try {
    const content = await getProcess()
    processList.value = content
    processListObj.value = arr2obj(content, 'id')
  } catch (error) {
    console.log(error, '获取所有工序失败')
  }
}

onMounted(() => {
  fetchPreloadData()
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.specPrefixStr = v.crossSectionPrefix.join(' / ')
    v.needProcessIds = artifactTypeListObj.value[v.classificationId].processIds
    v.processObj =
      (v.structureProcessPriceList?.length && arr2obj(v.structureProcessPriceList, 'processId')) ||
      artifactTypeListObj.value[v.classificationId].typeProcessObj
    return v
  })
}
</script>

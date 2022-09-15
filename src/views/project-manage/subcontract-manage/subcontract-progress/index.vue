<template>
  <div class="app-container">
    <div v-if="globalProject?.businessType===businessTypeEnum.INSTALLATION.V">
      <div style="margin-bottom:20px;">
        <el-tag v-for="item in supplierList" :key="item.supplierId" :type="crud.query.supplierId===item.supplierId?'success':''" :effect="crud.query.supplierId===item.supplierId?'light':'plain'" style="margin-right:10px;cursor:pointer;" size="medium" @click="choseSupplier(item)">{{item.supplierName}}</el-tag>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        :stripe="false"
        return-source-data
        :showEmptySymbol="false"
        row-key="rowKey"
        :cell-class-name="cellClassName"
        :row-class-name="handleRowClassName"
        style="width: 100%"
      >
          <el-table-column prop="index" label="序号" align="center" width="60">
          <template v-slot="scope">
            <span v-if="scope.row.showType===1">{{ scope.row.showIndex }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('monomerName')" align="center" key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体"/>
        <el-table-column v-if="columns.visible('type')" align="center" key="type" prop="type" :show-overflow-tooltip="true" label="项目内容">
          <template v-slot="scope">
            <span>{{scope.row.contentType?TechnologyTypeAllEnum.VL[scope.row.contentType]:'-'}}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('areaName')" align="center" key="areaName" prop="areaName" :show-overflow-tooltip="true" label="单元" min-width="150"/>
        <el-table-column v-if="columns.visible('produceArr')" align="center" key="produceArr" prop="produceArr" :show-overflow-tooltip="true" label="生产方式">
          <template v-slot="scope">
            <span v-for="(item,index) in scope.row.produceArr" :key="index">{{manufactureTypeEnum.VL[item]}}{{index!==scope.row.produceArr.length-1?'、':''}}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('installProgress')"
          key="installProgress"
          prop="installProgress"
          :show-overflow-tooltip="true"
          label="安装进度"
          min-width="220"
        >
          <template v-slot="scope">
            <template v-if="isNotBlank(scope.row.deepVal)">
              <template v-if="scope.row.showType===1">
                <div>{{`计划用时${scope.row.installVal?.totalDays || '-'}天|`}}<span :class="scope.row.installVal?.totalDays < scope.row.installVal?.actualDays?'red-color':'green-color'" style="margin-right:3px;">{{`已用时${scope.row.installVal?.actualDays || '-'}天`}}</span>{{`总量${scope.row.installVal?.mete.toFixed(scope.row.installVal.decimal) || '-'}${scope.row.installVal?.unit || ''}|已完成${scope.row.installVal?.completedMete.toFixed(scope.row.installVal?.decimal) || '-' }${scope.row.installVal?.unit || ''}`}}</div>
                <el-progress
                  v-if="isNotBlank(scope.row.installVal)"
                  :text-inside="true"
                  :stroke-width="26"
                  :percentage="scope.row.installVal?.meteRate"
                  :status="'success'"
                />
              </template>
              <template v-else>
                <div>{{`计划用时${scope.row.installVal?.totalDays || '-'}天|`}}<span :class="scope.row.installVal?.totalDays < scope.row.installVal?.actualDays?'red-color':'green-color'">{{`已用时${scope.row.installVal?.actualDays || '-'}天`}}</span></div>
                <el-progress
                  v-if="isNotBlank(scope.row.installVal)"
                  :text-inside="true"
                  :stroke-width="26"
                  :percentage="scope.row.installVal?.dayRate"
                  status="warning"
                />
              </template>
            </template>
            <template v-else>-</template>
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.detail])"
          label="操作"
          width="100px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <common-button
              v-if="checkPermission(permission.detail)"
              size="mini"
              icon="el-icon-view"
              type="primary"
              @click="openDetail(scope.row)"
            />
          </template>
        </el-table-column>
      </common-table>
    </div>
    <div v-else>
      <el-tag type="danger" size="medium" style="margin-bottom: 10px"> * 您好，请先选择业务类型为项目承包的项目，当前页面需要选择业务类型为项目承包方可查看 </el-tag>
    </div>
    <subcontractDetail v-model="detailVisible" :detailInfo="detailInfo"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/plan-progress'
import { supplierAccountList } from '@/api/project-manage/subcontract-plan'
import { ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import { mapGetters } from '@/store/lib'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
import { manufactureTypeEnum, areaPlanTypeEnum } from '@enum-ms/plan'
import { subcontractProgressPM as permission } from '@/page-permission/project'
import { dateDifferenceReduce } from '@/utils/date'
import { DP } from '@/settings/config'
import { isNotBlank } from '@/utils/data-type'

import subcontractDetail from './module/subcontract-detail'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const supplierList = ref([])
const detailInfo = ref({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '进度状态',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.subcontract-progress',
  paginate: true,
  extraHeight: 120
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      fetchList()
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function handleRowClassName({ row, rowIndex }) {
  return row.showType === 1 ? '' : 'abnormal-row'
}

function cellClassName({ row, rowIndex }) {
  return row.showType === 1 ? '' : 'abnormal-row'
}

function choseSupplier(val) {
  crud.query.supplierId = val.supplierId
}

// 获取分包商登录账号
async function fetchList() {
  if (!globalProjectId.value && globalProject.value.businessType !== businessTypeEnum.INSTALLATION.V) {
    return false
  }
  try {
    const { content = [] } = await supplierAccountList({ projectId: globalProjectId.value })
    supplierList.value = content
    crud.query.supplierId = supplierList.value.length ? supplierList.value[0].supplierId : undefined
  } catch (error) {
    console.log('获取分包账号失败', error)
  }
}

function openDetail(row) {
  detailInfo.value = {
    projectId: globalProjectId.value,
    supplierId: crud.query.supplierId,
    supplierName: crud.query.supplierId ? supplierList.value.find(v => v.supplierId === crud.query.supplierId).supplierName : undefined,
    monomerId: row.monomerId,
    areaId: row.showType === 2 ? row.id : undefined,
    type: row.showType === 2 ? row.productType : row.type
  }
  detailVisible.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProject.value.businessType === businessTypeEnum.INSTALLATION.V ? globalProjectId.value : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  const list = []
  let index = 1
  data.data.content.map(v => {
    if (v.monomerDetailList && v.monomerDetailList.length > 0) {
      v.monomerDetailList.map(k => {
        k.monomerName = v.name
        k.areaName = '全部'
        k.contentType = k.type
        k.produceArr = []
        k.showType = 1
        k.showIndex = index++
        k.rowKey = k.id
        const currentDate = new Date().getTime()
        if (k.detailTraceList && k.detailTraceList.length > 0) {
          k.detailTraceList.map((value, index) => {
            value.totalDays = value.startDate && value.endDate ? dateDifferenceReduce(value.startDate, value.endDate) : 0
            value.actualDays = value.startDate && value.startDate <= currentDate ? (value.completeDate ? dateDifferenceReduce(value.startDate, value.completeDate) : dateDifferenceReduce(value.startDate, currentDate)) : 0
            value.dayRate = value.totalDays ? Number((value.actualDays / value.totalDays * 100).toFixed(1)) : 0
            value.dayColor = '#1890ff'
            value.completedMete = value.completedMete || 0
            value.meteRate = value.completedMete && value.mete ? Number((value.completedMete / value.mete * 100).toFixed(1)) : 0
            value.meteColor = '#1890ff'
            value.monomerId = k.monomerId
            value.decimal = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? DP.COM_WT__KG : DP.MES_ENCLOSURE_L__M
            value.unit = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? 't' : 'm'
          })
        }
        const deepVal = k.detailTraceList.find(m => m.type === areaPlanTypeEnum.DEEPEN.V) || undefined
        const processVal = k.detailTraceList.find(m => m.type === areaPlanTypeEnum.PROCESS.V) || undefined
        const installVal = k.detailTraceList.find(m => m.type === areaPlanTypeEnum.INSTALL.V) || undefined
        k.deepVal = deepVal
        k.processVal = processVal
        k.installVal = installVal
        if (k.areaList && k.areaList.length > 0) {
          k.areaList.map((value, index) => {
            value.rowKey = k.id + '_' + value.id
            value.contentType = k.type
            value.areaName = value.name
            value.showType = 2
            value.produceArr = []
            value.produceArr.push(value.type)
            if (value.planDetailList && value.planDetailList.length > 0) {
              value.planDetailList.map((values, index) => {
                values.totalDays = values.startDate && values.endDate ? dateDifferenceReduce(values.startDate, values.endDate) : 0
                values.actualDays = values.startDate && values.startDate <= currentDate ? (values.completeDate ? dateDifferenceReduce(values.startDate, values.completeDate) : dateDifferenceReduce(values.startDate, currentDate)) : 0
                values.dayRate = values.totalDays ? Number((values.actualDays / values.totalDays * 100).toFixed(1)) : 0
                values.dayColor = '#1890ff'
                values.completedMete = values.completedMete || 0
                values.meteRate = values.completedMete && values.mete ? Number((values.completedMete / values.mete * 100).toFixed(1)) : 0
                values.meteColor = '#1890ff'
                values.monomerId = k.monomerId
                values.areaId = value.id
                values.decimal = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? DP.COM_WT__KG : DP.MES_ENCLOSURE_L__M
                values.unit = k.type === TechnologyTypeAllEnum.STRUCTURE.V ? 't' : 'm'
              })
            }
            const deepVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.DEEPEN.V)
            const processVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.PROCESS.V)
            const installVal = value.planDetailList.find(m => m.type === areaPlanTypeEnum.INSTALL.V)
            value.deepVal = deepVal || undefined
            value.processVal = processVal || undefined
            value.installVal = installVal || undefined
            if (k.produceArr.indexOf(value.type) < 0) {
              k.produceArr.push(value.type)
            }
          })
        }
        k.children = k.areaList || []
        list.push(k)
      })
    }
  })
  data.data.content = list
  return data
}

</script>
<style lang="scss" scoped>
::v-deep(.el-table .abnormal-row) {
  background: #f0f9eb;
}
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
::v-deep(.el-progress-bar__inner){
  text-align: center;
  max-width: 100%;
}
.red-color{
  color:red;
}
.green-color{
  color:green;
}
</style>

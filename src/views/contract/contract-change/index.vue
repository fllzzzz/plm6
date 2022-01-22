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
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('project.shortName')"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" :show-overflow-tooltip="true" label="内容" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.type ? contractChangeTypeEnum.VL[scope.row.type] : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('amount')"
        key="amount"
        prop="amount"
        :show-overflow-tooltip="true"
        label="金额(元)"
        min-width="180"
      >
        <template v-slot="scope">
          <span v-if="scope.row.type === contractChangeTypeEnum.CONTRACT_INFO.V">{{toThousand(scope.row.contractAmount)}}</span>
          <span v-if="scope.row.type === contractChangeTypeEnum.CONTRACT_SETTLE.V">{{toThousand(scope.row.settlementAmount)}}</span>
          <template v-if="scope.row.type === contractChangeTypeEnum.CONTRACT_AMOUNT.V || scope.row.type === contractChangeTypeEnum.VARIATION_ORDER.V">
            <span>{{toThousand(scope.row.contractAmount)}}</span>
            <span>{{scope.row.contractAmount<scope.row.changeAmount?'<':'>'}}</span>
            <span :class="scope.row.contractAmount>scope.row.changeAmount?'tip-red':'tip-green'">{{ toThousand(scope.row.changeAmount) }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('changeDate')"
        key="changeDate"
        prop="changeDate"
        :show-overflow-tooltip="true"
        label="变更日期"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.changeDate?parseTime(scope.row.changeDate,'{y}-{m}-{d}'):'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('changeDesc')"
        key="changeDesc"
        prop="changeDesc"
        :show-overflow-tooltip="true"
        label="变更描述"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.changeDesc?scope.row.changeDesc:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="创建日期"
        align="center"
        min-width="120"
      >
        <template v-slot="scope">
          <div>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        key="userName"
        prop="userName"
        label="发起人"
        align="center"
        width="90px"
        :show-overflow-tooltip="true"
      >
        <template v-slot="scope">
          <div>{{ scope.row.userName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('leaderList')"
        key="leaderList"
        prop="leaderList"
        label="签证人"
        align="center"
        width="110px"
        :show-overflow-tooltip="true"
      >
        <template v-slot="scope">
          <template v-if="contractChangeTypeEnum.VARIATION_ORDER.V">
            <div v-if="scope.row.leaderList && scope.row.leaderList.length>0"><span v-for="(item,index) in scope.row.leaderList" :key="item.id">{{ index!==0? ','+item.name: item.name }}</span></div>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditTime')"
        key="auditTime"
        prop="auditTime"
        label="审核日期"
        align="center"
        width="110px"
        :show-overflow-tooltip="true"
      >
        <template v-slot="scope">
           <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditorName')"
        key="auditorName"
        prop="auditorName"
        label="审核人"
        align="center"
        width="90px"
        :show-overflow-tooltip="true"
      >
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('source')"
        key="source"
        prop="source"
        label="来源"
        align="center"
        width="90px"
        :show-overflow-tooltip="true"
      >
        <template v-slot="scope">
          <div>{{ scope.row.source? systemTypeEnum.VL[scope.row.source]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditStatus')"
        key="auditStatus"
        prop="auditStatus"
        label="状态"
        align="center"
        width="90px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: '' }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" v-permission="permission.detail" @click="openDetail(scope.row, 'detail')"/>
          <common-button icon="el-icon-s-check" type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 金额变更 -->
    <money-form ref="moneyRef" :audit-status="auditStatus" :project-id="projectId" v-model="moneyVisible" :detailInfo="detailInfo" :show-type="showType" @success="crud.toQuery"/>
    <!-- 签证变更 -->
    <variationOrder ref="variationRef" :audit-status="auditStatus" :project-id="projectId" v-model="variationVisible" :detailInfo="detailInfo" :show-type="showType" @success="crud.toQuery"/>
    <!-- 结算填报 -->
    <settle-form ref="settleRef" :audit-status="auditStatus" :project-id="projectId" v-model="settleVisible" :detailInfo="detailInfo" :show-type="showType" @success="crud.toQuery"/>
    <!-- 项目审核 -->
    <contractInfo ref="contractInfoRef" :audit-status="auditStatus" :detailInfo="detailInfo" v-model="contractVisible" :show-type="showType" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/change-audit-log'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { auditTypeEnum, contractChangeTypeEnum, systemTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import moneyForm from '../info/money-form'
import variationOrder from '../info/variation-order'
import settleForm from '../info/settle-form'
import contractInfo from './module/contractInfo'
import { projectNameFormatter } from '@/utils/project'
import { contractChangePM as permission } from '@/page-permission/contract'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const moneyRef = ref()
const auditStatus = ref()
const showType = ref('detail')
const moneyVisible = ref(false)
const variationVisible = ref(false)
const settleVisible = ref(false)
const contractVisible = ref(false)
const detailInfo = ref({})
const contractInfoRef = ref()
const projectId = ref()
const { crud, columns } = useCRUD(
  {
    title: '变更管理',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractChange',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  auditStatus.value = row.auditStatus
  showType.value = type
  detailInfo.value = row
  projectId.value = row.project.id
  switch (row.type) {
    case (contractChangeTypeEnum.CONTRACT_AMOUNT.V):
      moneyVisible.value = true
      break
    case (contractChangeTypeEnum.CONTRACT_SETTLE.V):
      settleVisible.value = true
      break
    case (contractChangeTypeEnum.CONTRACT_INFO.V):
      contractVisible.value = true
      break
    case (contractChangeTypeEnum.VARIATION_ORDER.V):
      variationVisible.value = true
      break
    default:
      break
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
.tip-red{
  color:red;
}
.tip-green{
  color:#67c23a;
}
</style>

<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-radio-group v-model="query.status" size="small" class="filter-item"  @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <template v-for="item in projectStatusEnum.ENUM">
          <el-radio-button
            :key="item.V"
            :label="item.V"
            v-if="item.V!= projectStatusEnum.SETTLED.V"
          >
            {{ item.L }}
          </el-radio-button>
        </template>
      </el-radio-group>
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="settleChange"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width:100px!important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.businessType"
        :options="businessTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        placeholder="业务类型"
        class="filter-item"
        @change="businessChange"
      />
      <common-select
        v-model="query.projectType"
        :options="projectTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="项目类型"
        style="width:200px"
        @change="crud.toQuery"
      />
      <el-cascader
        ref="cascaderRef"
        v-model="query.projectContentId"
        placeholder="项目内容"
        :options="projectContentOption"
        class="filter-item"
        :props="cascaderProps"
        :show-all-levels="true"
        :clearable="true"
        style="width:200px"
        @change="crud.toQuery"
        filterable
      />
      <common-select
        type="enum"
        size="small"
        v-model="query.structureStatus"
        :options="structureTypeEnum.ENUM"
        class="filter-item"
        placeholder="结构类型"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <branch-company-select
        v-model="query.contractSignBodyId"
        class="filter-item"
        placeholder="合同签订主体"
        style="width: 200px"
        clearable
        @change="crud.toQuery"
      />
      <div>
        <el-input
          v-model.trim="query.noOrProjectName"
          size="small"
          placeholder="输入合同编号或项目简称"
          style="width: 200px;"
          class="filter-item"
          clearable
          @blur="crud.toQuery"
        />
        <el-input
          v-model.trim="query.signerName"
          size="small"
          placeholder="销售负责人"
          style="width: 180px;"
          class="filter-item"
          clearable
          @blur="crud.toQuery"
        />
        <rrOperation/>
      </div>
      <el-row v-loading="crud.loading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
        <el-col :span="4" class="card-panel-col">
          <Panel name="项目数" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.sumQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="加工订单" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.processQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="项目承包" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.jobQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="进行中" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.processingQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="已结算" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.settlementQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <Panel name="累计合同额" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.contractAmount || 0" :precision="2" />
        </el-col>
      </el-row>
      <crudOperation add-text="合同立项">
        <template #optRight>
          <template v-if="checkPermission(crud.permission.completeList.get)">
            <el-badge :value="outFinishCount" :max="99" :hidden="outFinishCount < 1">
              <common-button size="mini" type="primary" @click="completeVisible=true" class="filter-item">可完工项目</common-button>
            </el-badge>
          </template>
        </template>
        <template #viewLeft>
          <print-table
            v-permission="crud.permission.print"
            api-key="projectList"
            :params="{year: crud.query.year}"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </template>
      </crudOperation>
    </div>
    <common-drawer
      title="可完工项目"
      v-model="completeVisible"
      :append-to-body="true"
      :show-close="true"
      :close-on-click-modal="false"
      direction="rtl"
      size="80%"
      :before-close="
        () => {
          completeVisible = false
        }
      "
    >
      <template #content>
        <completeList @success="handleSuccess"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { defineProps, ref, watch, defineEmits, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { projectStatusEnum, projectTypeEnum, businessTypeEnum, structureTypeEnum } from '@enum-ms/contract'
import { settlementStatusEnum } from '@enum-ms/finance'
import { getContentInfo } from '@/api/contract/project'
import { ElRadioGroup } from 'element-plus'
import completeList from './complete-list'
import { completeData, projectNumData } from '@/api/contract/project'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import Panel from '@/components/Panel'

// const projectContentOption = ref([])
let machiningData = []
let installData = []
const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    expandTrigger: 'hover',
    emitPath: false,
    multiple: false,
    checkStrictly: false
  }
})
const completeVisible = ref(false)
const defaultQuery = {
  projectType: undefined, year: undefined, noOrProjectName: undefined, businessType: undefined, projectContentId: undefined,
  signerName: '',
  status: projectStatusEnum.PROCESS.V,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  contractSignBodyId: undefined
}
const outFinishCount = ref()
const totalAmount = ref({})
const emit = defineEmits(['projectChange'])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  currentProjectType: {
    type: [Number, String],
    default: undefined
  }
})
const projectContentOption = computed(() => {
  if (query.businessType && query.projectType) {
    console.log(machiningData)
    switch (query.projectType) {
      case projectTypeEnum.STEEL.V:
        return query.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.STEEL.V] : installData[projectTypeEnum.STEEL.V]
      case projectTypeEnum.BRIDGE.V:
        return query.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.BRIDGE.V] : installData[projectTypeEnum.BRIDGE.V]
      default: return query.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.CARBARN.V] : installData[projectTypeEnum.CARBARN.V]
    }
  } else {
    return []
  }
})
watch(
  () => props.currentProjectType,
  (val) => {
    if (val) {
      crud.query.projectType = val
      crud.toQuery()
    }
  },
  { immediate: true }
)

contentInfo()

async function contentInfo() {
  try {
    machiningData = await getContentInfo({ businessType: businessTypeEnum.MACHINING.V })
    installData = await getContentInfo({ businessType: businessTypeEnum.INSTALLATION.V })
    const dataArr = [machiningData, installData]
    for (let i = 0; i < dataArr.length; i++) {
      if (dataArr[i] && dataArr[i][projectTypeEnum.STEEL.V].length > 0) {
        dataArr[i][projectTypeEnum.STEEL.V].map(v => {
          v.name = v.categoryName
        })
      }
      if (dataArr[i] && dataArr[i][projectTypeEnum.BRIDGE.V].length > 0) {
        dataArr[i][projectTypeEnum.BRIDGE.V].map(v => {
          v.name = v.categoryName
        })
      }
      if (dataArr[i] && dataArr[i][projectTypeEnum.CARBARN.V].length > 0) {
        dataArr[i][projectTypeEnum.CARBARN.V].map(v => {
          v.name = v.categoryName
        })
      }
    }
  } catch (error) {
    console.log(error)
  }
}

function businessChange() {
  crud.toQuery()
}

function handleSuccess() {
  completeVisible.value = false
  crud.toQuery()
  emit('projectChange')
}

getCompleteData()

async function getCompleteData() {
  try {
    const data = await completeData()
    outFinishCount.value = data.outFinishCount || 0
  } catch (error) {
    console.log('获取完工列表', error)
  }
}

getProjectNumData()

async function getProjectNumData() {
  try {
    const data = await projectNumData()
    totalAmount.value = data || {}
  } catch (error) {
    console.log('获取项目数量', error)
  }
}

function settleChange() {
  if (crud.query.settlementStatus === settlementStatusEnum.SETTLED.V) {
    crud.query.status = undefined
  }
  crud.toQuery()
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:left;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 18px;
        text-align:right;
      }
    }
  }
}
</style>

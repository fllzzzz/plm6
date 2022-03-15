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
      <el-radio-group v-model="query.settlementStatus" size="small" class="filter-item"  @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button
          v-for="item in settlementStatusEnum.ENUM"
          :key="item.V"
          :label="item.V"
        >
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
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
      <common-select
        v-model="query.projectContentId"
        :options="projectContentOption"
        :type="'other'"
        :dataStructure="typeProp"
        size="small"
        clearable
        class="filter-item"
        filterable
        placeholder="项目内容"
        style="width:200px"
        @change="crud.toQuery"
      />
      <div>
        <el-input
          v-model="query.noOrProjectName"
          size="small"
          placeholder="输入合同编号或项目简称"
          style="width: 200px;"
          class="filter-item"
          clearable
          @blur="crud.toQuery"
        />
        <el-input
          v-model="query.signerName"
          size="small"
          placeholder="输入签约人"
          style="width: 120px;"
          class="filter-item"
          clearable
          @blur="crud.toQuery"
        />
        <rrOperation/>
      </div>
      <crudOperation add-text="合同立项">
        <template #viewLeft>
          <common-button size="mini" type="primary" class="filter-item" @click="completeVisible=true" v-permission="crud.permission.completeList">可完工项目</common-button>
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
import { defineProps, ref, watch, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { projectStatusEnum, settlementStatusEnum, projectTypeEnum, businessTypeEnum } from '@enum-ms/contract'
import { getContentInfo } from '@/api/contract/project'
import { ElRadioGroup } from 'element-plus'
import completeList from './complete-list'

const projectContentOption = ref([])
let projectContent1 = []
let projectContent2 = []
const typeProp = { key: 'id', label: 'name', value: 'id' }
const completeVisible = ref(false)
const defaultQuery = {
  projectType: undefined, year: undefined, noOrProjectName: undefined, businessType: undefined, projectContentId: undefined,
  signerName: '',
  status: projectStatusEnum.PROCESS.V,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V
}
const emit = defineEmits(['projectChange'])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  currentProjectType: {
    type: [Number, String],
    default: undefined
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
    const options = []
    const data1 = await getContentInfo({ businessType: businessTypeEnum.ENUM.MACHINING.V })
    const data2 = await getContentInfo({ businessType: businessTypeEnum.ENUM.INSTALLATION.V })
    if (data1 && data1.projectContentVOList.length > 0) {
      data1.projectContentVOList.forEach(v => {
        if (v.contentList.length > 0) {
          v.contentList.forEach(k => {
            k.alias = v.type
            options.push(k)
          })
        }
      })
    }
    projectContent1 = options || []
    projectContent2 = data2.projectContentVOList || []
  } catch (error) {
    console.log(error)
  }
}

function businessChange() {
  crud.query.projectContent = undefined
  if (crud.query.businessType) {
    projectContentOption.value = crud.query.businessType === 1 ? projectContent1 : projectContent2
  } else {
    projectContentOption.value = []
  }
  crud.toQuery()
}

function handleSuccess() {
  completeVisible.value = false
  crud.toQuery()
  emit('projectChange')
}
</script>

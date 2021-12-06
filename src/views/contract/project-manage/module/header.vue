<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.status"
        :options="projectStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width:100px!important"
        placeholder="选择年"
        value-format="yyyy"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.noOrProjectName"
        size="small"
        placeholder="输入合同编号或项目类型"
        style="width: 200px;"
        class="filter-item"
        clearable
        @keyup.enter.native="crud.toQuery"
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
        :options="projectTypeEnumN.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="项目类型"
        style="width:200px"
      />
      <common-select
        v-model="query.projectContent"
        :options="projectContentOption"
        :type="'other'"
        :dataStructure="typeProp"
        size="small"
        clearable
        class="filter-item"
        placeholder="项目类型"
        style="width:200px"
      />
      <div>
        <el-input
          v-model="query.singerName"
          size="small"
          placeholder="输入签约人"
          style="width: 120px;"
          class="filter-item"
          clearable
          @keyup.enter.native="crud.toQuery"
        />
        <rrOperation/>
      </div>
      <crudOperation add-text="合同立项">
        <!-- <template slot="optRight">
          <el-button type="info" size="mini" icon="el-icon-time" @click="changeLogVisible = true">项目变更记录</el-button>
        </template>
        <template slot="viewLeft">
          <print-table
            v-permission="permission.print"
            api-key="CONTRACT_LEDGER"
            :params="{year:query.year}"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </template> -->
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { projectStatusEnum, settlementStatusEnum, projectTypeEnumN, businessTypeEnum } from '@enum-ms/contract'
import { getContentInfo } from '@/api/contract/project'

const projectContentOption = ref([])
let projectContent1 = []
let projectContent2 = []
const typeProp = { key: 'id', label: 'name', value: 'id' }
const defaultQuery = {
  projectType: undefined, year: undefined, noOrProjectName: undefined, businessType: undefined, projectContent: undefined,
  singerName: '',
  status: projectStatusEnum.PROCESS.V,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V
}

const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  currentProjectType:{
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

function businessChange(){
  crud.query.projectContent = undefined
  if (crud.query.businessType) {
    projectContentOption.value = crud.query.businessType === 1 ? projectContent1 : projectContent2
  } else {
    projectContentOption.value = []
  }
}
</script>

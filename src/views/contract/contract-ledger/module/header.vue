<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
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
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { settlementStatusEnum, projectTypeEnumN, businessTypeEnum } from '@enum-ms/contract'
import { getContentInfo } from '@/api/contract/project'
import { ElRadioGroup } from 'element-plus'

const projectContentOption = ref([])
let projectContent1 = []
let projectContent2 = []
const typeProp = { key: 'id', label: 'name', value: 'id' }
const defaultQuery = {
  projectId: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  projectType: undefined,
  businessType: undefined,
  projectContent: undefined
}

const monomerSelectRef = ref()
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
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
    projectContentOption.value = crud.query.businessType === businessTypeEnum.ENUM.MACHINING.V ? projectContent1 : projectContent2
  } else {
    projectContentOption.value = []
  }
}
</script>

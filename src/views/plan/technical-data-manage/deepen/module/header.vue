<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
       @change="monomerChange"
      />
      <common-radio-button
        v-model="query.productType"
        :options="typeOption"
        :type="'other'"
        :dataStructure="typeProp"
        class="filter-item"
        @change="crud.toQuery"
      />
      <div>
        <el-input
          v-model="query.serialNumber"
          placeholder="输入编号搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter.native="crud.toQuery"
        />
        <el-input
          v-model="query.fileName"
          placeholder="输入文件名搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter.native="crud.toQuery"
        />
        <rrOperation/>
      </div>
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          ref="deepenRef"
          v-if="crud.query.monomerId"
          v-permission="crud.permission.import"
          :disabled="!query.monomerId"
          :upload-fun="upload"
          :data="carryParam"
          btn-name="图纸上传"
          btn-type="warning"
          btn-size="mini"
          icon="el-icon-upload"
          :accept="accept"
          class="filter-item"
          :material-type="crud.query.type"
          @success="crud.toQuery"
        />
      </template>
      <!-- <template #viewLeft>
        <common-button
          v-if="projectId"
          v-permission="permission.download"
          :loading="downloadLoading"
          type="warning"
          icon="el-icon-download"
          size="mini"
          :disabled="!projectId"
          @click.stop="downloadAll()"
        >下载项目下所有文件</common-button>
      </template> -->
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import { monomerDetail } from '@/api/plan/monomer'
import { processingEnum } from '@enum-ms/plan'
import { planTypeEnum } from '@enum-ms/plan'
import uploadBtn from '../../components/drawing-upload-btn'
import { upload } from '@/api/plan/technical-data-manage/deepen'


const router = useRouter()

const defaultQuery = {
  drawingNumber: undefined, serialNumber: undefined, fileName: undefined,
  monomerId: { value: undefined, resetAble: false },
  type: { value: planTypeEnum.ARTIFACT.V, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const typeOption = ref([])
const carryParam = ref({})
const deepenRef = ref()
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

async function monomerChange() {
  try {
    if (crud.query.monomerId) {
      await getTypeInfo()
    } else {
      typeOption.value = []
    }
  } catch (e) {
    console.log(e)
  } finally {
    crud.toQuery()
  }
}

async function getTypeInfo() {
  try {
    const { productTypeList } = await monomerDetail(crud.query.monomerId)
    typeOption.value = []
    const option = []
    if (productTypeList && productTypeList.length > 0) {
      productTypeList.forEach(v => {
        if (v.no !== 5) {
          option.push(v)
        }
      })
      typeOption.value = option
      typeOption.value.unshift({
        name: '构件',
        no: 7
      }, { name: '零件',
        no: 8 })
    }
    crud.query.productType = typeOption.value.length > 0 ? typeOption.value[0].no : undefined
    typeChange(crud.query.productType)
  } catch (e) {
    console.log(e)
  }
}

function typeChange(val) {
  if (val) {
    switch (val) {
      case 7:
        crud.query.type = 0
        crud.query.enclosureCategory = 0
        break
      case 8:
        crud.query.type = 1
        crud.query.enclosureCategory = 0
        break
      default:
        crud.query.type = 2
        crud.query.enclosureCategory = crud.query.productType
        break
    }
    carryParam.value = {
      monomerId: crud.query.monomerId,
      type: crud.query.type,
      enclosureCategory: crud.query.enclosureCategory
    }
    crud.toQuery()
  } else {
    crud.query.type = undefined
    crud.query.enclosureCategory = undefined
  }
}

const accept=computed(()=>{
  if (crud.query.type === planTypeEnum.ENCLOSURE.V) {
    return '.pdf,.zip,.jpg,.jpeg,.png'
  } else {
    return '.pdf,.zip'
  }
})
</script>

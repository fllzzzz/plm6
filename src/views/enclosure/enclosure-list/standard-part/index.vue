<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" :typeOption="typeOption" @enclosurePlan="enclosurePlanChange"/>
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        return-source-data
        style="width: 100%"
        @sort-change="crud.handleSortChange"
        class="enclosure-table"
        :cell-class-name="wrongCellMask"
      >
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column
        prop="name"
        label="名称"
        align="center"
        fixed="left"
        min-width="150"
        v-if="columns.visible('name')"
        :show-overflow-tooltip="true"
      >
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.name" type="text" placeholder="名称" style="width:100%" maxlength="20"/>
          <span v-else>{{row.name}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specification')" :show-overflow-tooltip="true" prop="specification" label="规格" align="center" min-width="200px">
        <template #default="{ row }">
          <el-input v-if="row.isModify" v-model.trim="row.specification" type="text" placeholder="规格" style="width:100%" maxlength="20"/>
          <span v-else>{{row.specification}}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('measureUnit')" :show-overflow-tooltip="true" prop="measureUnit" label="单位" align="center" min-width="70px" />
      <el-table-column v-if="columns.visible('quantity')" :show-overflow-tooltip="true" prop="quantity" label="数量" align="center">
        <template #default="{ row }">
          <common-input-number
            v-if="row.isModify"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            size="mini"
            placeholder="数量"
          />
          <!-- <common-input-number
            v-if="row.isModify"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            size="mini"
            placeholder="数量"
            @change="weightChange(row)"
          /> -->
          <span v-else>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
          v-if="columns.visible('accountingUnit')"
          :show-overflow-tooltip="true"
          prop="accountingUnit"
          label="核算单位"
          align="center"
          min-width="180px"
        >
          <template #default="{ row }">
            <div v-if="row.isModify" style="display: flex; justify-content: center; align-items: center;">
              <div>
                <el-input v-if="!row.boolWeightTypeEnum" v-model.trim="row.accountingUnit" placeholder="核算单位" type="text" style="width: 100px" maxlength="20" />
                <common-select
                  v-else
                  style="width: 100px"
                  v-model="row.accountingUnit"
                  placeholder="核算单位"
                  :options="weightUnit"
                  :data-structure="{ key: 'value', label: 'label', value: 'value' }"
                />
              </div>
              <common-select
                v-model="row.boolWeightTypeEnum"
                style="width: 100px; margin-left: 10px"
                :options="accountingUnitValue"
                :data-structure="{ key: 'value', label: 'label', value: 'value' }"
              />
            </div>
            <span v-else>{{ row.accountingUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('mete')"
          :show-overflow-tooltip="true"
          prop="mete"
          label="核算量"
          align="center"
          min-width="120px"
        >
          <template #default="{ row }">
            <common-input-number
              v-if="row.isModify"
              v-model="row.mete"
              placeholder="核算量"
              size="mini"
              :controls="false"
              :precision="3"
              :step="0.001"
              :min="0"
              :max="9999999"
            />
            <span v-else>{{ row.mete }}</span>
          </template>
        </el-table-column>
      <!-- <el-table-column label="单重(kg)" prop="weight">
        <template #default="{ row }">
          <el-input-number
            v-if="row.isModify"
            v-model.number="row.weight"
            :min="0"
            :max="999999999"
            :step="1"
            :precision="DP.COM_WT__KG"
            :controls="false"
            placeholder="单重"
            style="width:100%;"
            @change="weightChange(row)"
          />
          <span v-else>{{toThousand(row.weight,DP.COM_WT__KG)}}</span>
        </template>
      </el-table-column>
      <el-table-column label="总重(kg)" prop="totalWeight">
        <template #default="{ row }">
          <span>{{toThousand(row.totalWeight,DP.COM_WT__KG)}}</span>
        </template>
      </el-table-column> -->
      <el-table-column v-if="columns.visible('useProperty')" prop="useProperty" label="使用范围" align="center" min-width="120px">
      <template #default="{ row }">
        <common-select
          v-if="row.isModify"
          v-model="row.useProperty"
          :options="auxiliaryMaterialUseTypeEnum.ENUM"
          type="enum"
          size="small"
          clearable
          placeholder="使用范围"
        />
        <span v-else>{{ auxiliaryMaterialUseTypeEnum.VL[row.useProperty] }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="remark" label="备注" align="center" :show-overflow-tooltip="true" min-width="120px">
      <template #default="{ row }">
        <el-input
          v-if="row.isModify"
          v-model.trim="row.remark"
          type="textarea"
          :autosize="{ minRows: 1, maxRows: 6 }"
          :maxlength="200"
          placeholder="备注"
          style="width:100%"
        />
        <span v-else>{{ row.remark }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="boolReturn" label="是否退量" align="center">
      <template v-slot="scope">
        <el-switch
          v-if="scope.row.isModify"
          v-model="scope.row.boolReturn"
          active-color="#13ce66"
          :active-value="true"
          :inactive-value="false"
        />
        <span v-else :style="`color:${scope.row.boolReturn?'green':''}`">{{scope.row.boolReturn?'√':'-'}}</span>
      </template>
    </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <template v-if="scope.row.isModify">
            <common-button type="info" size="mini" @click="rowCancel(scope.row)">取消</common-button>
            <common-button type="primary" size="mini" @click="rowSubmit(scope.row)">保存</common-button>
          </template>
          <template v-else>
            <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.row)" v-permission="permission.edit"/>
            <!-- <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon-color="red"
              title="确定删除吗?"
              @confirm="deleteRow(scope.row)"
              v-if="checkPermission(permission.del)"
            >
              <template #reference>
                <common-button size="small" class="el-icon-delete" type="danger"/>
              </template>
            </el-popconfirm> -->
            <common-button size="small" class="el-icon-view" type="success" @click="deleteRow(scope.row)"></common-button>
          </template>
        </template>
      </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm :enclosurePlan="enclosurePlan" />
      <!-- 围护变更 -->
      <enclosureChange v-model:visible="deleteVisible" :detailInfo="currentRow" />
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">当前项目内容没有包含围护制品，请到合同管理中进行配置</span>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/enclosure-plan/standard-part'
import { watch, provide, ref } from 'vue'

// import { DP } from '@/settings/config'
// import { toThousand } from '@/utils/data-type/number'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'
import { validate } from '@compos/form/use-table-validate'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import enclosureChange from '../components/enclosure-change.vue'
import { mapGetters } from '@/store/lib'
import { ElMessage } from 'element-plus'
import { enclosureStandardPartPM as permission } from '@/page-permission/enclosure'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const originRow = ref({})

const typeOption = ref([])
const pageShow = ref(false)
const deleteVisible = ref(false)
const currentRow = ref({})
const enclosurePlan = ref([])
const techOptions = [
  {
    name: '压型彩板',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '夹芯板',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  }
]
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护配套件清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.plan-standard-part',
  paginate: true,
  extraHeight: 40
})

const accountingUnitValue = ref([
  {
    value: true,
    label: '重量'
  },
  {
    value: false,
    label: '其他'
  }
])

const weightUnit = ref([
  {
    value: '吨',
    label: '吨'
  },
  {
    value: '千克',
    label: '千克'
  }
])

watch(
  () => globalProject.value,
  (val) => {
    typeOption.value = []
    if (isNotBlank(val)) {
      techOptions.forEach((v) => {
        if (val.projectContentList.findIndex((k) => Number(k.no) === v.no) > -1) {
          typeOption.value.push(v)
        }
      })
      pageShow.value = typeOption.value.length > 0
    }
  },
  { deep: true, immediate: true }
)

provide('globalProject', globalProject)

// function weightChange(row) {
//   row.totalWeight = (row.quantity && row.weight) ? row.quantity * row.weight : 0
// }

const tableRules = {
  useProperty: [{ required: true, message: '请输入选择使用范围', trigger: 'change' }],
  name: [{ required: true, message: '请输入名称', trigger: 'blur' }],
  specification: [{ required: true, message: '请输入规格', trigger: 'blur' }],
  measureUnit: [{ required: true, message: '请输入单位', trigger: 'blur' }],
  quantity: [{ required: true, message: '请输入数量', trigger: 'change' }]
}

function enclosurePlanChange(val) {
  enclosurePlan.value = val
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

function editRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
}
// async function deleteRow(row) {
//   try {
//     await crudApi.del([row.id])
//     crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
//     crud.toQuery()
//   } catch (e) {
//     console.log('删除', e)
//   }
// }

const deleteRow = (row) => {
  currentRow.value = row
  deleteVisible.value = true
}
function rowCancel(row) {
  row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  row.isModify = false
}

async function rowSubmit(row) {
  const rules = tableRules
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  try {
    await crudApi.edit(row)
    crud.notify(`修改成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    row.isModify = false
  } catch (e) {
    console.log(`修改`, e)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content.map(v => {
    v.boolReturn = v.boolReturn || false
    v.projectId = v.project.id
    return v
  })
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
</style>

<template>
  <div class="head-container">
    <div v-show="crud.props.searchToggle">
      <monomer-select
        ref="monomerSelect"
        :value.sync="query.monomerId"
        :project-id="projectId"
        class="filter-item"
      />
      <!--      <common-radio-button-->
      <!--        :value.sync="query.status"-->
      <!--        :options="processingEnum"-->
      <!--        show-all-->
      <!--        type="enum"-->
      <!--        class="filter-item"-->
      <!--        @change="crud.toQuery"-->
      <!--      />-->
      <!--      <material-class-cascader-->
      <!--        :value.sync="query.typeArr"-->
      <!--        :basic-class="basicClassEnum.MATERIAL.V"-->
      <!--        check-strictly-->
      <!--        filterable-->
      <!--        clearable-->
      <!--        show-unit-->
      <!--        show-code-->
      <!--        class="filter-item"-->
      <!--        style="width: 400px;"-->
      <!--        @change="handleTypeChange"-->
      <!--      />-->
      <!--      <el-input-->
      <!--        v-model="query.color"-->
      <!--        placeholder="输入颜色搜索"-->
      <!--        class="filter-item"-->
      <!--        style="width: 200px;"-->
      <!--        size="small"-->
      <!--        clearable-->
      <!--        @keyup.enter.native="crud.toQuery"-->
      <!--      />-->
      <!--      <el-input-->
      <!--        v-model="query.specification"-->
      <!--        placeholder="输入规格搜索"-->
      <!--        class="filter-item"-->
      <!--        style="width: 200px;"-->
      <!--        size="small"-->
      <!--        clearable-->
      <!--        @keyup.enter.native="crud.toQuery"-->
      <!--      />-->
      <!--      <rrOperation :crud="crud" />-->
    </div>
    <crudOperation>
      <template slot="optLeft">
        <el-button
          v-permission="permission.add"
          class="filter-item"
          size="mini"
          type="primary"
          icon="el-icon-plus"
          @click.stop="addDlgVisible = true"
        >
          新增
        </el-button>
      </template>
    </crudOperation>
    <mAddForm :visible.sync="addDlgVisible" :area-id="currentArea.id" @success="handleAddSuccess" />
  </div>
</template>

<script>
import { listUpload, changeListUpload, clearAll } from '@/api/mes-plan/technical-manage/auxiliary-material'
import CRUD, { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
import mAddForm from './form-add'
import monomerSelect from '@/views/components/plan/monomer-select'
// import materialClassCascader from '@/views/components/base/material-class-cascader'
import { materialTypeEnum, materialBasicClassEnum as basicClassEnum, processingEnum } from '@/utils/enum/index'

const defaultQuery = {
  color: undefined, specification: undefined, typeArr: undefined,
  firstId: undefined, secondId: undefined, thirdId: undefined,
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false }
}
export default {
  components: { mAddForm, crudOperation, monomerSelect },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  props: {
    projectId: {
      type: [Number, String],
      default: undefined
    }
  },
  data() {
    return {
      processingEnum,
      materialTypeEnum,
      basicClassEnum,
      currentArea: {},
      addDlgVisible: false
    }
  },
  computed: {
    carryParam() {
      return { areaId: this.query.areaId }
    }
  },
  methods: {
    listUpload,
    changeListUpload,
    clearAll,
    handleAddSuccess() {
      this.$notify({ title: '新增成功', type: 'success' })
      this.crud.toQuery()
    },
    handleTypeChange(val) {
      this.query.firstId = undefined
      this.query.secondId = undefined
      this.query.thirdId = undefined
      val && val.forEach((v, i) => {
        if (i === 0) {
          this.query.firstId = v && v.id
        }
        if (i === 1) {
          this.query.secondId = v && v.id
        }
        if (i === 2) {
          this.query.thirdId = v && v.id
        }
      })
      this.crud.toQuery()
    },
    [CRUD.HOOK.handleRefresh](crud, data) {
      data.content = data.content.map(v => {
        v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
        return v
      })
    },
    tabClick(val) {
      this.mismatchList = ''
      const { name, label } = val
      this.currentArea = {
        id: name,
        name: label
      }
      this.crud.toQuery()
    }
  }
}
</script>

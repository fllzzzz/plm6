<template>
  <span>
    <span>{{ formatValue }}</span>
    <span v-if="showUnit">{{ unit }}</span>
  </span>
</template>

<script>
import convert from 'convert-units'

export default {
  props: {
    from: {
      type: String,
      default: 'g'
    },
    to: {
      type: String,
      default: 'mt'
    },
    value: {
      type: [String, Number],
      default: 0
    },
    decimals: {
      type: [Number],
      default: 0
    },
    showUnit: {
      type: Boolean,
      default: false
    }
  },
  data() {
    return {
      //   formatValue: 0
    }
  },
  computed: {
    formatValue() {
      /**
       * 公吨是公制的单位,中国采用公制,所以我们中国人平常说的"吨"指的就是"公吨",可把"吨"看作是"公吨"的简称
       * 1公吨（tonne/metric ton）= 1000公斤
       * 而在英美,"吨"是不大一样的
       * 1公吨（tonne/metric ton）= 1000公斤
       * 1吨(ton）= 1016公斤（英）或907.2公斤（美）
       * 那么我们学习的英文里吨就是ton,这怎么解释呢?
       * 因为1公吨在英文中原本的表达法为tonne或者metric ton,由于用公吨的人太多,人都喜欢偷懒,故常把metric ton缩略为ton.所以外国人说ton的时候,有可能是指metric ton(公吨),也可能指在自己国家的ton(吨),而我们中国人说ton(吨),其实指的都是公吨.
       * 在我国,1公吨=1吨
       * 在英国,美国,1公吨近似于但不等于1吨
       */
      let num = this.value
      if (num === undefined || num === null || isNaN(+num)) return num
      if (!this.from || !this.to) return num
      num = convert(num)
        .from(this.from)
        .to(this.to)
      num = num.toFixed(this.decimals)
      return num
    },
    unit() {
      if (this.to === 'mt') {
        return 't'
      } else {
        return this.to
      }
    }
  },
  methods: {}
}
</script>
